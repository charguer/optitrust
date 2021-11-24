open Optitrust
open Target
open Ast

let main = cFunDef "main"

let dims = ["X";"Y";"Z"]
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims

let _ = Run.script_cpp ~inline:["particle_chunk.h";"particle_chunk_alloc.h";"particle.h"] (fun () ->
  
  
  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (var "nbCells") [main;cVarDef "nextCharge"];
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [occIndex 1;main; cFor "k"]; (* TODO: place a label earlier on, on the relevant loop *)
  (* TODO: read_last_write   on  .. = nextCharge[MINDEX1(nbCells, idCell)]    to become  .. = 0 *)
  (* TODO: below Lit_int 0 should be Lit_double 0 *)
  (* TODO: put in some library:   let delocalize_double_add = Delocalize_arith (Lit_double 0, Binop_add)
     then use [delocalize_double_add] here and further on as argument *)
  !! Matrix_basic.delocalize ~dim:(var "nbCorners") ~index:"k" ~acc:"sum" ~ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cMark "first_local"]; (* TODO: ~init_zero:true
       so no need to generate nextChargeCorners[MINDEX2(nbCorners, nbCells, 0, idCell)] = nextCharge[MINDEX1(nbCells, idCell)]; *)
  !! Variable.inline [main; cVarDef "indices"];
  !! Specialize.any "k" [cAny];
  let my_bij_code =
    "int mybij(int nbCells, int nbCorners, int idCell, int idCorner) {
      coord coord = coordOfCell(idCell);
      int ix = coord.ix;
      int iy = coord.iy;
      int iz = coord.iz;
      int res[] = {
        cellOfCoord(ix, iy, iz),
        cellOfCoord(ix, iy, wrap(gridZ,iz-1)),
        cellOfCoord(ix, wrap(gridY,iy-1), iz),
        cellOfCoord(ix, wrap(gridY,iy-1), wrap(gridZ,iz-1)),
        cellOfCoord(wrap(gridX,ix-1), iy, iz),
        cellOfCoord(wrap(gridX,ix-1), iy, wrap(gridZ,iz-1)),
        cellOfCoord(wrap(gridX,ix-1), wrap(gridY,iy-1), iz),
        cellOfCoord(wrap(gridX,ix-1), wrap(gridY,iy-1), wrap(gridZ,iz-1)),
      };
     return MINDEX2(nbCells, nbCorners, res[idCorner], idCorner);
     }" in
  !! Sequence.insert (Ast.code my_bij_code) [tBefore; main];
  !! Matrix.biject "mybij" [occIndex 0; main; cFor "k"; cFun "MINDEX2"]; (* TODO: target should be  cellReadOrWrite ~base:"nextChargeCorners"  ->  on the base argument of the read/write -> check it is a mindex_ then replace it *)
  !! Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]];  (* TODO:  cLabel "initNextCharge"  ;  assuming ~labels:["initNextCharge",""] to be given to delocalize on nextCharnge *)
  !! Instr.replace (Ast.code "MINDEX2(nbCells, nbCorners, idCell2, k)") [cFun "mybij"]; (* ARTHUR: fixed when the rest is updated *)

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert ~name:"nbProcs" ~typ:"int" ~value:"8" [tBefore; main];
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [occIndex 1; main; cFor "k"]; (* TODO: use a label that should be on that loop *)
     Matrix_basic.delocalize ~dim:(var "nbCorners") ~index:"k" ~acc:"sum" ~ops:(Delocalize_arith (Lit_int 0, Binop_add))[cMark "first_local"];
     Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]]; (* TODO: use a label that should be on that loop, introduced by the earlier delocalize *)
     Specialize.any "k" [cAny]; (* this should be specialized not to k but to [myThread] *)

(* TODO: capitalize rest of the script *)

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Sequence.intro ~mark:"temp_seq" ~start:[main; cVarDef "coef_x0"] ~nb:6 (); (* TODO: replace 6 with (2*nb_dims),  where let nb_dims = List.length dims should be define at the top of the file *)
     Instr.move_invariant ~dest:[tBefore; main] [cMark "temp_seq"]; (* TODO: rename "move_invariant" to "move_out" *)
     Sequence.elim [cMark "temp_seq"]; (* TODO: rename "temp_seq" to "coefs" *)
     (* TODO: move_invariant would often apply to sequences, thus we could add an optional argument ?(elim_seq:bool=false) to perform the sequence elimination on the fly *)
  !! Loop.fission [tBefore; cVarDef "px"];
     Loop.fission [tBefore; main; cVarDef "ix"];
     Loop.hoist [cVarDef "idCell2"]; (* TODO: hoisting before fission *)

  (* Part: Coloring *)
  let sized_dims = [("ix", "gridX"); ("iy", "gridY"); ("iz", "gridZ")] in
  let dims = List.map fst sized_dims in
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "b" ^ d in
    Loop_basic.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor d]; (* DONE: ~index:"b${id}" *)
    Loop_basic.color color ~index:("c"^d) [cFor bd]
    in
  !! List.iter (colorize "2" "2") dims;
     Loop.reorder ~order:(Tools.((add_prefix "c" dims) @ (add_prefix "b" dims) @ dims)) [cFor "cix"];

  (* Introduction of the computation *)

  !! Variable.insert_list ~defs:[("int","blockSize","2"); ("int","2","blockSize / 2")] [tBefore; cVarDef "nbCells"]; (* TODO: put in the form ~defs[("int", ...] *)
      (* TODO: "2","blockSize / 2" does not seem right, because "2" is not a variable name...was it d? *)
     Variable.insert ~typ:"bool" ~name:"distanceToBlockLessThanHalfABlock" ~value:"(ix >= bix - d && ix < bix + blockSize + d)&& (iy >= biy - d && iy < biy + blockSize + d) && (iz >= biz - d && iz < biz + blockSize + d)" [tAfter; main; cVarDef "iz"];
     (* TODO  assume "d" is rename to "dist";  then we can make above shorter:
         Variable.insert (Ast.trm_ands (map_dims (fun d -> expr ~vars:[d] "(i${1} >= bi${1} - dist && i${1} < bi${1} + blockSize + dist)"))))
         where the "value" argument needs not use a label since it has type trm directly
         and where trm_ands is a smart construction for building a conjunction from a list of terms *)
     Instr.replace (var "distanceToBlockLessThanHalfABlock") [cFun "ANY_BOOL"];


  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bX";"bY";"bZ"]] [tBefore; cFor "biX"];

  (* Part: optimize chunk allocation *)  (* ARTHUR *)
  (* skip #16 *)

)

