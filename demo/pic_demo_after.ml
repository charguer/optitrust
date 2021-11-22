open Optitrust
open Target
open Ast

let main = cFunDef "main"
let dims = ["X";"Y";"Z"]
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims

let _ = Run.script_cpp (fun () ->

  (* LATER: ~parser:Clang_full | Clang | Menhir *)
  (* Part: scaling of speeds and positions #7 *)
  !! Variable.insert_list ~reparse:true ~typ:"const double"
        ~defs:(("factor", "particleCharge * stepDuration * stepDuration / particleMass")
              :: map_dims (fun d -> ("factor" ^ d), ("factor / cell" ^ d)) ) [tBefore; cVarDef "nbSteps"];

   (* TODO
   let cdouble = "const double" in
   Variable.insert_list ~reparse:true ~defs:(
         [cdouble, "factor", "particleCharge * stepDuration * stepDuration / particleMass"]
       @ (map_dims (fun d -> cdouble, ("factor" ^ d), ("factor / cell" ^ d))))
     [tBefore; cVarDef "nbSteps"];
  *)

  (* Part: scaling of field, speeds and positions *)
  !! iter_dims (fun d ->
       Accesses.scale ~factor_ast:(Ast.trm_var ("factor" ^ d)) [cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]); (* ARTHUR: needs compensation *)
  !! Variable.inline [cOr [[cVarDef "accel"]; [cVarDef ~regexp:true "factor."] (*; [cVarDef "factor"]*)]];
  !!! Variable.inline [cVarDef "factor"]; (* TODO: see why occurrence not found on the previous line *)
  (* LATER: variable.inline_at which takes only the occurrence and finds automatically the source *)
  !! iter_dims (fun d ->
       Accesses.scale (* TODO ~reparse:false *) ~factor:("stepDuration / cell" ^ d) [nbMulti; cFieldReadOrWrite ~field:("speed" ^ d) ()]);
  !! iter_dims (fun d ->
       Accesses.scale ~factor:("1. / cell" ^ d) [nbMulti; cFieldReadOrWrite ~field:("pos" ^ d) ()]);

  (* Part: grid_enumeration *)
  !! Loop.grid_enumerate (map_dims (fun d -> ("i" ^ d,"grid" ^ d))) [cFor "idCell" ~body:[cWhile ()]];

  (* Part: Shifting of positions*)
  !! iter_dims (fun d ->
    Instr.inline_last_write ~write:[cWriteVar ("fieldAtPos" ^ d)] [cVarDef "accel"; cRead ~addr:[cVar ("fieldAtPos" ^ d)] ()]);


  (* TODO :ARTHUR : see how to inline the zero for fieldatpos in the simplest way *)
  !! Variable.inline [cVarDef ~regexp:true "fieldAtPos."];
  !! iter_dims (fun d ->
      Variable.bind_intro ~fresh_name:("p" ^ d) [cFor "i"; cStrict; cFieldWrite ~field:("pos"^d) ();  dRHS]);

  !! Instr.(gather ~dest:(GatherAt [tBefore; sInstr "= pX"])) [main; cVarDef ~regexp:true "p."]; (* TODO: rename to gather_targets *)


  !! iter_dims (fun d ->
    Accesses.shift ~factor_ast:(Ast.trm_var ("i" ^  d)) [cOr [[cWrite ~lhs:[sExpr ("(c->items)[i].pos"^d)] ()]; [cVarDef ("p" ^ d); cRead ~addr:[sExpr ("(c->items)[i].pos" ^ d )] ()]]];);

  (* Part: convert pos fields to float *)
  !! Cast.insert ~typ_ast:(Ast.typ_float ()) [sExprRegexp ~substr:true "\\(p. \\+ i.\\)"];

  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [main;cVarDef "nextCharge"];
     Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [occIndex 1;main; cFor "k"];
     Matrix_basic.delocalize ~dim:(Ast.trm_var "nbCorners") ~index:"k" ~acc:"sum" [cMark "first_local"];
     Variable.inline [main; cVarDef "indices"];
     Specialize.any "k" [cAny];
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
     Sequence.insert (Ast.code my_bij_code) [tBefore;main];
     Matrix.biject "mybij" [occIndex 0;main; cFor "k" ; cFun "MINDEX2"];
     Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]];
     Instr.replace (Ast.code "MINDEX2(nbCells, nbCorners, idCell2,k)") [cFun "mybij"];

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert ~name:"nbProcs" ~typ:"int" ~value:"8" [tBefore; main];
     Matrix.local_name ~my_mark:"second_local" ~var:"nextChargeCorners" ~local_var:"nextChargeProCorners" ~indices:["idProc";"idCell"] [occIndex 2;main; cFor "k"];
     Matrix_basic.delocalize ~dim:(Ast.trm_var "nbProcs") ~index:"k" ~acc:"sum" [cMark "second_local"];
     Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]];
     Specialize.any "k" [cAny];

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Sequence.intro ~mark:"temp_seq" ~start:[main;cVarDef "coef_x0"] ~nb:6 ();
     Instr.move_invariant ~dest:[tBefore; main] [cMark "temp_seq"];
     Sequence.elim [cMark "temp_seq"];
     Loop.fission [tBefore; cVarDef "px"];
     Loop.fission [tBefore; main; cVarDef "ix"];
     Loop.hoist [cVarDef "idCell2"];


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

  !! Variable.insert_list ~defs:[("blockSize","2");("2","blockSize / 2")] ~typ:"int" [tBefore; cVarDef "nbCells"];
     Variable.insert ~name:"distanceToBlockLessThanHalfABlock" ~typ:"bool"  ~value:"(ix >= bix + d && ix < bix + blockSize + d)&& (iy >= biy + d && iy < biy + blockSize + d) && (iz >= biz + d && iz < biz + blockSize + d)" [tAfter; main; cVarDef "iz"];
     Instr.replace (Ast.trm_var "distanceToBlockLessThanHalfABlock") [cFun "ANY_BOOL"];


  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bx";"by";"bz"]] [tBefore; cFor "bix"];

  (* Part: optimize chunk allocation *)  (* ARTHUR *)
  (* skip #16 *)


)



(* --------------TODO: CHECK THIS IS DONE

  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [main;cVarDef "nextCharge"];
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [main; cFor "ix"];
  !! Matrix_basic.delocalize ~dim:(Ast.trm_var "nbCorners") ~index:"k" ~acc:"sum" [cMark "first_local"]; (* LATER: combi version *)
     Specialize.any "k" [cAny]; (* TODO: ~last:true in delocalize *)
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
  !! Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]]; (* TODO: instead of occINdex, specify cWrite ~lhs:[cVar "nextCharge"]*)
  !! Instr.replace (Ast.code "MINDEX2(nbCells, nbCorners, idCell, k)") [cFun "mybij"]; (* TODO: will be removed when biject is fixed *)

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert ~name:"nbProcs" ~typ:"int" ~value:"8" [tBefore; main];
     Matrix.local_name ~my_mark:"second_local" ~var:"nextChargeCorners" ~local_var:"nextChargeProcCorners" ~indices:["idProc";"idCell"] [main; cFor "ix"];
     Matrix_basic.delocalize ~dim:(Ast.trm_var "nbProcs") ~index:"k" ~acc:"sum" [cMark "second_local"];
     (* TODO: Specialize.any "idThread" [cAny]; *)
     (* TODO: rename Proc to Thread everywhere *)
  !! Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]]; (*  TODO:   cWrite ~lhs:[cVar "nextChargeCorners"]*)
  (* TODO:
    and adding of omp parallel instructions
  and moving nextChargeCorners and nextChargeProcCorners allocation/free outside of the loop
  and adding " const int idThread = omp_get_thread_num();"
  and specializnig the ANY value to idThread
  *)

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Sequence.intro ~mark:"temp_seq" ~start:[main; cVarDef "coef_x0"] ~nb:6 (); (* LATER/ ARTHUR think of a simpler way *)
     Instr.move_invariant ~dest:[tBefore; main] [cMark "temp_seq"];
     Sequence.elim [cMark "temp_seq"];

  !! Loop.fission [tBefore; cVarDef "px"];
  !! Loop.fission [tBefore; main; cVarDef "ix"];
  !! Loop.hoist [cVarDef "idCell2"]; (* TODO: should hoist before second split *) (* TODO: clarify ix and ix2 *)

  (* Part: Coloring *)
  !! let sized_dims = [("ix", "gridX"); ("iy", "gridY"); ("iz", "gridZ")] in (* TODO: use map_dims *)
     let idims = List.map fst sized_dims in
     let colorize (tile : string) (color : string) (d:string) : unit =
        let bd = "b" ^ d in
        Loop_basic.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor d]; (* DONE: ~index:"b${id}" *)
        Loop_basic.color color ~index:("c"^d) [cFor bd]
        in
     List.iter (colorize "2" "2") idims; (* TODO: replace 2 with blocksize, which should be introduced earlier *)
     Loop.reorder ~order:(Tools.((add_prefix "c" idims) @ (add_prefix "b" idims) @ idims)) [cFor "cix"];

  (* Introduction of the computation *)
  !! Variable.insert_list ~names:["blockSize";"d"] ~values:["2";"blockSize / 2"] ~typ:"int" [tBefore; cVarDef "nbCells"]; (* TODO: use ~defs *)
  !! Variable.insert ~name:"distanceToBlockLessThanHalfABlock" ~typ:"bool"  ~value:"(ix >= bix + d && ix < bix + blockSize + d)&& (iy >= biy + d && iy < biy + blockSize + d) && (iz >= biz + d && iz < biz + blockSize + d)" [tAfter; main; cVarDef "iz"]; (* TODO: target should be tBefore; cIf ~arg:[cAny]*)
     Instr.replace (Ast.trm_var "distanceToBlockLessThanHalfABlock") [cFun "ANY_BOOL"]; (* TODO: Specialize.any_bool *)


  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti;tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bx";"by";"bz"]] [tBefore; cFor "bix"];

  (* Part: optimize chunk allocation *)
  (* skip #16 *)
-------------------*)
