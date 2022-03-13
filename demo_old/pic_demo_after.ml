open Optitrust
open Target
open Ast

let main = cFunDef "main"
let dims = ["X";"Y";"Z"]
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims

let _ = Run.script_cpp (fun () ->




)



(* --------------TODO: CHECK THIS IS DONE

  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [main;cVarDef "nextCharge"];
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [main; cFor "ix"];
  !! Matrix_basic.delocalize ~dim:(Ast.trm_var "nbCorners") ~index:"k" ~acc:"sum" [cMark "first_local"]; (* LATER: combi version *)
     Specialize.any "k" [cAny]; (* DONE: ~last:true in delocalize *)
  !! Variable.inline [main; cVarDef "indices"];
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
        cesqr



        llOfCoord(wrap(gridX,ix-1), iy, iz),
        cellOfCoord(wrap(gridX,ix-1), iy, wrap(gridZ,iz-1)),
        cellOfCoord(wrap(gridX,ix-1), wrap(gridY,iy-1), iz),
        cellOfCoord(wrap(gridX,ix-1), wrap(gridY,iy-1), wrap(gridZ,iz-1)),
      };
     return MINDEX2(nbCells, nbCorners, res[idCorner], idCorner);
     }" in
     Sequence.insert (Ast.code my_bij_code) [tBefore; main];
  !! Matrix.biject "mybij" [occIndex 0; main; cFor "k"; cFun "MINDEX2"]; (* TODO: see pic_rest2, add unit test *)
  !! Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]]; (* DONE: instead of occINdex, specify cWrite ~lhs:[cVar "nextCharge"]*)
  !! Instr.replace (Ast.code "MINDEX2(nbCells, nbCorners, idCell, k)") [cFun "mybij"]; (* TODO: will be removed when biject is fixed *)

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert ~name:"nbProcs" ~typ:"int" ~value:"8" [tBefore; main];
     Matrix.local_name ~my_mark:"second_local" ~var:"nextChargeCorners" ~local_var:"nextChargeProcCorners" ~indices:["idProc";"idCell"] [main; cFor "ix"];
     Matrix_basic.delocalize ~dim:(Ast.trm_var "nbProcs") ~index:"k" ~acc:"sum" [cMark "second_local"];
     (* DONE: Specialize.any "idThread" [cAny]; *)
     (* DONE: rename Proc to Thread everywhere *)
  !! Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]]; (*  DONE:   cWrite ~lhs:[cVar "nextChargeCorners"]*)
  (* DONE:
    and adding of omp parallel instructions
  and moving nextChargeCorners and nextChargeProcCorners allocation/free outside of the loop
  and adding " const int idThread = omp_get_thread_num();"
  and specializnig the ANY value to idThread
  *)

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Sequence.intro ~mark:"temp_seq" ~start:[main; cVarDef "coef_x0"] ~nb:6 (); (* LATER/ ARTHUR think of a simpler way *)
     Instr.move_out ~dest:[tBefore; main] [cMark "temp_seq"];
     Sequence.elim [cMark "temp_seq"];

  !! Loop.fission [tBefore; cVarDef "px"];
  !! Loop.fission [tBefore; main; cVarDef "ix"];
  !! Loop.hoist [cVarDef "idCell2"]; (* DONE: should hoist before second split *) (* DONE: clarify ix and ix2 *)

  (* Part: Coloring *)
  !! let sized_dims = [("ix", "gridX"); ("iy", "gridY"); ("iz", "gridZ")] in (* DONE: use map_dims *)
     let idims = List.map fst sized_dims in
     let colorize (tile : string) (color : string) (d:string) : unit =
        let bd = "b" ^ d in
        Loop_basic.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor d]; (* DONE: ~index:"b${id}" *)
        Loop_basic.color color ~index:("c"^d) [cFor bd]
        in
     List.iter (colorize "2" "2") idims; (* DONE: replace 2 with blocksize, which should be introduced earlier *)
     Loop.reorder ~order:(Tools.((add_prefix "c" idims) @ (add_prefix "b" idims) @ idims)) [cFor "cix"];

  (* Introduction of the computation *)
  !! Variable.insert_list ~names:["blockSize";"d"] ~values:["2";"blockSize / 2"] ~typ:"int" [tBefore; cVarDef "nbCells"]; (* DONE: use ~defs *)
  !! Variable.insert ~name:"distanceToBlockLessThanHalfABlock" ~typ:"bool"  ~value:"(ix >= bix + d && ix < bix + blockSize + d)&& (iy >= biy + d && iy < biy + blockSize + d) && (iz >= biz + d && iz < biz + blockSize + d)" [tAfter; main; cVarDef "iz"]; (* TODO: target should be tBefore; cIf ~arg:[cAny]*)
     Instr.replace (Ast.trm_var "distanceToBlockLessThanHalfABlock") [cFun "ANY_BOOL"]; (* DONE: Specialize.any_bool *)


  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti;tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bx";"by";"bz"]] [tBefore; cFor "bix"];

  (* Part: optimize chunk allocation *)
  (* skip #16 *)
-------------------*)