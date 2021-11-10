open Optitrust
open Target
open Ast

let main = cFunDef "main"

let _ = Run.script_cpp (fun () ->



  (* Part: scaling of speeds and positions #7 *)
  !! Variable.insert ~name:"factor"  ~typ:"const double" ~value:"particleCharge * stepDuration * stepDuration /particleMass / cellX" [tBefore; cVarDef "nbSteps"];
  !! Variable.insert ~name:"factorX" ~typ:"const double" ~value:"factor / cellX" [tAfter; cVarDef "factor"];
  !! Variable.insert ~name:"factorY" ~typ:"const double" ~value:"factor / cellY" [tAfter; cVarDef "factorX"];
  !! Variable.insert ~name:"factorZ" ~typ:"const double" ~value:"factor / cellZ" [tAfter; cVarDef "factorY"];
  (* TODO: insert and insert_list should have option ~reparse:false

         let defs = ["factor", "particleCharge * stepDuration * stepDuration /particleMass / cellX";
                     "factorX", "factor / cellX";
                     "factorY", ...] in
         let defs = List.map (fun d -> "factor" ^ d, "factor / cell" ^ d) dims in
      !! Variable.insert_list ~typ:"const double" ~values:defs [tBefore; cVarDef "nbSteps"];
      *)


  (*

    Tools.list_iter dims ~f:(fun d ->
     Accesses.scale ~factor_ast:(Ast.trm_var ("factor" ^ (Tools.uppercase d))) [cVarDef "accel"; cReadVar ("fieldAtPos_" ^ d)]);
  *)
  !! Accesses.scale ~factor_ast:(Ast.trm_var "factorX") [cVarDef "accel"; cReadVar "fieldAtPos_x"];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "factorY") [cVarDef "accel"; cReadVar "fieldAtPos_y"];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "factorZ") [cVarDef "accel"; cReadVar "fieldAtPos_z"];
  (* TODO: target all the reads of the right form in the labelled sequence p_write: {..}  added before set_explicit *)
  !! Accesses.scale ~factor_ast:(Ast.trm_var "stepDuration / cellX") [nbMulti;main; cWrite ~lhs:[sExpr "(c->items)[i].speed"] ();cRead ~addr:[sExpr "(c->items)[i].speed.x"] ()];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "stepDuration / cellY") [nbMulti;main; cWrite ~lhs:[sExpr "(c->items)[i].speed"] ();cRead ~addr:[sExpr "(c->items)[i].speed.y"] ()];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "stepDuration / cellZ") [nbMulti;main; cWrite ~lhs:[sExpr "(c->items)[i].speed"] ();cRead ~addr:[sExpr "(c->items)[i].speed.z"] ()];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "1. / cellX") [nbMulti;main; cWrite ~lhs:[sExpr "(c->items)[i].pos"] ();cRead ~addr:[sExpr "(c->items)[i].pos.x"] ()];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "1. / cellY") [nbMulti;main; cWrite ~lhs:[sExpr "(c->items)[i].pos"] ();cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
  !! Accesses.scale ~factor_ast:(Ast.trm_var "1. / cellZ") [nbMulti;main; cWrite ~lhs:[sExpr "(c->items)[i].pos"] ();cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()];

  (* TODO: arthur will give the pseudo code for the automated simplifier *)


  (* Part: grid_enumeration *)
  !! Loop.grid_enumerate [("ix", "gridSize"); ("iy", "gridSize"); ("iz", "gridSize")] [tIndex ~nb:3 1;cFor "idCell"];


  (* Part: Shifting of positions*)
  !! Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_x"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_x"] ()];
  !! Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_y"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_y"] ()];
  !! Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_z"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_z"] ()];

(* TODO :ARTHUR : see how to inline the zero for fieldatpos in the simplest way *)
  !! Variable.inline [nbMulti;cVarDef ~regexp:true "fieldAtPos_."];
  !! Variable.inline [nbMulti; main; cVarDef "accel"];
  !! Variable.bind_intro ~fresh_name:"px" [sInstr "(c->items)[i].pos.x ="; dRHS];
  !! Variable.bind_intro ~fresh_name:"py" [sInstr "(c->items)[i].pos.y ="; dRHS];
  !! Variable.bind_intro ~fresh_name:"pz" [sInstr "(c->items)[i].pos.z ="; dRHS];
  !! Instr.move_multiple ~destinations:[[tAfter; cVarDef "px"];[tAfter; cVarDef "py"]] ~targets:[[cVarDef "py"];[cVarDef "pz"]];

  !! Accesses.shift ~factor_ast:(Ast.trm_var "ix") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.x"] ()]; [cVarDef "px"; cRead ~addr:[sExpr "(c->items)[i].pos.x"] ()]]];
  !! Accesses.shift ~factor_ast:(Ast.trm_var "iy") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.y"] ()]; [cVarDef "py"; cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()]]];
  !! Accesses.shift ~factor_ast:(Ast.trm_var "iz") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.z"] ()]; [cVarDef "pz"; cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()]]];

  (* Part: convert pos fields to float *)
  !! Cast.insert ~typ_ast:(Ast.typ_float ()) [sInstr "(c->items)[i].pos.x ="; dRHS];
  !! Cast.insert ~typ_ast:(Ast.typ_float ()) [sInstr "(c->items)[i].pos.y ="; dRHS];
  !! Cast.insert ~typ_ast:(Ast.typ_float ()) [sInstr "(c->items)[i].pos.z ="; dRHS];

  (* Part: AOS-SOA *)
  !! Struct.inline "speed" [cTypDef "particle"];
  !! Struct.inline "pos" [cTypDef "particle"];

  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [cVarDef "nextCharge"];
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [tIndex 1;main; cFor "k"];
  !! Matrix_basic.delocalize ~dim:(Ast.trm_var "nbCorners") ~index:"k" ~acc:"sum" [cMark "first_local"];
  !! Variable.inline [cVarDef "indices1"];
  !! Specialize.any "k" [cAny];
  let my_bij_code =
    "int mybij(int nbCells, int nbCorners, int idCell, int idCorner) {
      coord coord = coordOfCell(idCell);
      int ix = coord.ix;
      int iy = coord.iy;
      int iz = coord.iz;
      int res[] = {
        cellOfCoord(ix, iy, iz),
        cellOfCoord(ix, iy, wrapX(gridZ,iz-1)),
        cellOfCoord(ix, wrapX(gridY,iy-1), iz),
        cellOfCoord(ix, wrapX(gridY,iy-1), wrapX(gridZ,iz-1)),
        cellOfCoord(wrapX(gridX,ix-1), iy, iz),
        cellOfCoord(wrapX(gridX,ix-1), iy, wrapX(gridZ,iz-1)),
        cellOfCoord(wrapX(gridX,ix-1), wrapX(gridY,iy-1), iz),
        cellOfCoord(wrapX(gridX,ix-1), wrapX(gridY,iy-1), wrapX(gridZ,iz-1)),
      };
     return MINDEX2(nbCells, nbCorners, res[idCorner], idCorner);
     }" in
    Sequence.insert (Ast.code my_bij_code) [tBefore;main];
!!! Matrix.biject "mybij" [tIndex 0;main; cFor "k" ; cFun "MINDEX2"];
  !! Instr.delete [tIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]];
  !! Instr.replace (Ast.code "MINDEX2(nbCells, nbCorners, idCell2,k)") [cFun "mybij"];

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert ~name:"nbProcs" ~typ:"int" ~value:"8" [tBefore; main];
  !! Matrix.local_name ~my_mark:"second_local" ~var:"nextChargeCorners" ~local_var:"nextChargeProCorners" ~indices:["idProc";"idCell"] [tIndex 2;main; cFor "k"];
  !! Matrix_basic.delocalize ~dim:(Ast.trm_var "nbProcs") ~index:"k" ~acc:"sum" [cMark "second_local"];
  !! Instr.delete [tIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]];
  !! Specialize.any "k" [cAny];

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Sequence.intro ~mark:"temp_seq" ~start:[main;cVarDef "coef_x"] ~nb:6 ();
  !! Instr.move_invariant ~dest:[tBefore; main] [cMark "temp_seq"];
  !! Sequence.elim [cMark "temp_seq"];
  !! Loop.fission [tBefore; cVarDef "px"];
  !! Loop.fission [tBefore; cVarDef "ix2"];
  !! Loop.hoist [cVarDef "idCell2"];


  (* Part: Coloring *)
     let sized_dims = [("ix", "gridX"); ("iy", "gridY"); ("iz", "gridZ")] in
     let dims = List.map fst sized_dims in
     let colorize (tile : string) (color : string) (d:string) : unit =
      let bd = "b" ^ d in
      Loop_basic.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor d]; (* DONE: ~index:"b${id}" *)
      Loop_basic.color color ~index:("c"^d) [cFor bd]
        in
  !! List.iter (colorize "2" "2") dims;
  !! Loop.reorder ~order:(Tools.((add_prefix "c" dims) @ (add_prefix "b" dims) @ dims)) [cFor "cix"];

  (* Introduction of the computation *)
  !! Variable.insert ~name:"blockSize" ~typ:"int" ~value:"2" [tAfter; cVarDef "gridSize"];
  !! Variable.insert ~name:"d" ~typ:"int" ~value:"blockSize / 2" [tAfter;cVarDef "blockSize"];
  !! Variable.insert ~name:"distanceToBlockLessThanHalfABlock" ~typ:"bool"  ~value:"(ix >= bix + d && ix < bix + blockSize + d)&& (iy >= biy + d && iy < biy + blockSize + d) && (iz >= biz + d && iz < biz + blockSize + d)" [tAfter; cVarDef "rz1"];
  !! Instr.replace (Ast.trm_var "distanceToBlockLessThanHalfABlock") [cFun "ANY_BOOL"];


  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti;tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
  !! Omp.parallel_for [Shared ["bx";"by";"bz"]] [tBefore; cFor "bix"];

  (* Part: optimize chunk allocation *)
  (* skip #16 *)

)
