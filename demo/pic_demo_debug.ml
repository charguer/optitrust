open Optitrust
open Target
open Ast 

let _ = Run.script_cpp (fun () ->
  

  (* Part: Shifting of positions*)
  !! Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_x"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_x"] ()];
     Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_y"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_y"] ()];
     Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_z"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_z"] ()];
     
     Variable.inline [nbMulti;cVarDef ~regexp:true "fieldAtPos_."];
     Variable.inline [nbMulti; cFunDef "main"; cVarDef "accel"];
     Variable.bind_intro ~fresh_name:"px" [sInstr "(c->items)[i].pos.x ="; dRHS];
     Variable.bind_intro ~fresh_name:"py" [sInstr "(c->items)[i].pos.y ="; dRHS];
     Variable.bind_intro ~fresh_name:"pz" [sInstr "(c->items)[i].pos.z ="; dRHS];
     Instr.move_multiple ~destinations:[[tAfter; cVarDef "px"];[tAfter; cVarDef "py"]] ~targets:[[cVarDef "py"];[cVarDef "pz"]];
     
     Accesses.shift (Ast.trm_var "ix") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.x"] ()]; [cVarDef "px"; cRead ~addr:[sExpr "(c->items)[i].pos.x"] ()]]];
     Accesses.shift (Ast.trm_var "iy") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.y"] ()]; [cVarDef "py"; cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()]]];
     Accesses.shift (Ast.trm_var "iz") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.z"] ()]; [cVarDef "pz"; cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()]]];

  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [cVarDef "nextCharge"];
     Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [tIndex 1;cFunDef "main"; cFor "k"];
     Matrix_basic.delocalize ~dim:(Ast.trm_var "nbCorners") ~index:"k" ~acc:"sum" [cMark "first_local"];
     Variable.inline [cVarDef "indices1"];
     Specialize.any "k" [cAny];
  let my_bij_code = 
    "int mybij(int nbCells, int nbCorners, int idCell, int idCorner) {
      coord coord = coordOfCell(idCell);
      int ix = coord.ix;
      int iy = coord.iy; 
      int iz = coord.iz;
      int result[] = {
        cellOfCoord(ix, iy, iz),
        cellOfCoord(ix, iy, wrapX(gridZ,iz-1)),
        cellOfCoord(ix, wrapX(gridY,iy-1), iz),
        cellOfCoord(ix, wrapX(gridY,iy-1), wrapX(gridZ,iz-1)),
        cellOfCoord(wrapX(gridX,ix-1), iy, iz),
        cellOfCoord(wrapX(gridX,ix-1), iy, wrapX(gridZ,iz-1)),
        cellOfCoord(wrapX(gridX,ix-1), wrapX(gridY,iy-1), iz),
        cellOfCoord(wrapX(gridX,ix-1), wrapX(gridY,iy-1), wrapX(gridZ,iz-1)),
      };
     return MINDEX2(nbCells, nbCorners, result[idCorner], idCorner);
     }" in
    Sequence.insert (Ast.code my_bij_code) [tBefore;cFunDef "main"];
!!! Matrix.biject "mybij" [tIndex 0;cFunDef "main"; cFor "k" ; cFun "MINDEX2"];
    Instr.delete [tIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]];

    (* TODO: This is probably not correct *)
    Instr.replace (code "MINDEX2(nbCells, nbCorners, idCell2,k)") [cFun "mybij"];

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert "nbProcs" "int" "8" [tBefore; cFunDef "main"];
     Matrix.local_name ~my_mark:"second_local" ~var:"nextChargeCorners" ~local_var:"nextChargeProCorners" ~indices:["idProc";"idCell"] [tIndex 2;cFunDef "main"; cFor "k"];
     Matrix_basic.delocalize ~dim:(Ast.trm_var "nbProcs") ~index:"k" ~acc:"sum" [cMark "second_local"];     
     Instr.delete [tIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]];
     Specialize.any "k" [cAny];

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Loop.invariant [cFunDef "main"; cVarDef "coef_x"];
     Loop.invariant [cFunDef "main"; cVarDef "coef_y"];
     Loop.invariant [cFunDef "main"; cVarDef "coef_z"];
     Loop.invariant [cFunDef "main"; cVarDef "sign_x"];
     Loop.invariant [cFunDef "main"; cVarDef "sign_y"];
     Loop.invariant [cFunDef "main"; cVarDef "sign_z"];
    
     Loop.fission [tBefore; cVarDef "px"];
     Loop.fission [tBefore; cVarDef "ix2"];
     Loop.hoist [cVarDef "idCell2"];


  (* Part: Coloring *)
     let sized_dims = [("ix", "gridX"); ("iy", "gridY"); ("iz", "gridZ")] in
     let dims = List.map fst sized_dims in
     let colorize (tile : string) (color : string) (d:string) : unit =
     let bd = "b" ^ d in
     Loop_basic.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor d]; (* DONE: ~index:"b${id}" *)
     Loop_basic.color color ~index:("c"^d) [cFor bd]
      in
     List.iter (colorize "2" "2") dims;
  !! Loop.reorder ~order:(Tools.((add_prefix "c" dims) @ (add_prefix "b" dims) @ dims)) [cFor "cix"];
  
  (* Introduction of the computation *)
  !! Variable.insert "blockSize" "int" "2" [tAfter; cVarDef "gridSize"];
     Variable.insert "d" "int" "blockSize / 2" [tAfter;cVarDef "blockSize"];
     Variable.insert "distanceToBlockLessThanHalfABlock" "bool"  "(ix >= bix + d && ix < bix + blockSize + d)&& (iy >= biy + d && iy < biy + blockSize + d) && (iz >= biz + d && iz < biz + blockSize + d)" [tAfter; cVarDef "rz1"];
     Instr.replace (Ast.trm_var "distanceToBlockLessThanHalfABlock") [cFun "ANY_BOOL"];
  
  
  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti;tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bx";"by";"bz"]] [tBefore; cFor "bix"]; 
)
