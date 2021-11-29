open Optitrust
open Target
open Ast

let main = cFunDef "main"
let dims = ["X";"Y";"Z"]
let nb_dims = List.length dims
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims
let idims = map_dims (fun d -> "i" ^ d)
let delocalize_double_add = Delocalize_arith (Lit_double 0., Binop_add)

let _ = Run.script_cpp ~inline:["particle_chunk.h";"particle_chunk_alloc.h";"particle.h"] (fun () ->
  
  
  (* Part: duplication of corners for vectorization of change deposit *)
  !! Label.add "charge" [main; cFor "k" ~body:[cVar "nextCharge"]];
  !! Matrix.intro_mops (var "nbCells") [main;cVarDef "nextCharge"];
  !! Matrix.local_name ~my_mark:"charge" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [cLabel "charge"];
  !! Matrix_basic.delocalize ~init_zero:true ~dim:(var "nbCorners") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add [cMark "charge"]; 
  !! Variable.inline [main; cVarDef "indices"];
  !! Specialize.any "k" [cAny];
  let my_bij_code =
    "int mybij(int nbCells, int nbCorners, int idCell, int idCorner) {
      coord coord = coordOfCell(idCell);
      int iX = coord.iX;
      int iY = coord.iY;
      int iZ = coord.iZ;
      int res[] = {
        cellOfCoord(iX, iY, iZ),
        cellOfCoord(iX, iY, wrap(gridZ,iZ-1)),
        cellOfCoord(iX, wrap(gridY,iY-1), iZ),
        cellOfCoord(iX, wrap(gridY,iY-1), wrap(gridZ,iZ-1)),
        cellOfCoord(wrap(gridX,iX-1), iY, iZ),
        cellOfCoord(wrap(gridX,iX-1), iY, wrap(gridZ,iZ-1)),
        cellOfCoord(wrap(gridX,iX-1), wrap(gridY,iY-1), iZ),
        cellOfCoord(wrap(gridX,iX-1), wrap(gridY,iY-1), wrap(gridZ,iZ-1)),
      };
     return MINDEX2(nbCells, nbCorners, res[idCorner], idCorner);
     }" in
  !! Sequence.insert (stmt my_bij_code) [tBefore; main];
  !! Matrix.biject "mybij" [main; cVarDef "nextChargeCorners"];
  !! Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]];  (* TODO:  cLabel "initNextCharge"  ;  assuming ~labels:["initNextCharge",""] to be given to delocalize on nextCharnge *)
  !! Instr.replace ~reparse:true (stmt "MINDEX2(nbCells, nbCorners, idCell2, k)") [cFun "mybij"];

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert ~name:"nbProcs" ~typ:"int" ~value:(lit "8") [tBefore; main];
  !! Matrix.local_name ~my_mark:"cores" ~var:"nextChargeCorners" ~local_var:"nextChargeProCorners" ~indices:["idProc";"idCell"] [cLabel "charge"];
  !! Matrix_basic.delocalize ~init_zero:true ~dim:(var "nbProcs") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add [cMark "cores"];
  !! Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]];
  !! Specialize.any "k" [cAny];

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Instr.move_out ~dest:[tBefore; main] [nbMulti; main; cVarDef ~regexp:true "\\(coef\\|sign\\).0"];
  !! Loop.hoist [cVarDef "idCell2"]; 
  !! Loop.fission [tBefore; main; cVarDef "pX"];
  (* !! Loop.fission [tBefore; main; cVarDef "idCell2"]; *) (* TODO: Find the right place where the second split should be done *)

  (* Introduction of the computation *)
  !! Variable.insert_list ~defs:[("int","blockSize","2"); ("int","dist","blockSize / 2")] [tBefore; cVarDef "nbCells"]; 
  !! Variable.insert ~typ:"bool" ~name:"distanceToBlockLessThanHalfABlock" ~value:(trm_ands (map_dims (fun d -> expr ~vars:[d] "i${0} >= bi${0} - dist && i${0} < bi${0} + blockSize + dist"))) [tAfter; main; cVarDef "iZ2"];
  !! Specicalize.any "distanceToBlockLessThanHalfABlock" [cFun "ANY_BOOL"];
  
  (* Part: Coloring *)
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "bi" ^ d in
    Loop.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor ("i" ^ d)];
    Loop.color color ~index:("ci"^d) [cFor bd]
    in
  !! iter_dims (fun d -> colorize "blockSize" "blockSize" d);
    Loop.reorder ~order:(Tools.((add_prefix "c" idims) @ (add_prefix "b" idims) @ idims)) [cFor "ciX"];

  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bX";"bY";"bZ"]] [tBefore; cFor "biX"];

  (* Part: optimize chunk allocation *)  (* ARTHUR *)
  (* skip #16 *)

)

