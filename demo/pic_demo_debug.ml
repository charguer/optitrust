open Optitrust
open Target
open Ast 

let _ = Run.script_cpp (fun () ->
  
  (* Part: scaling of speeds and positions #7 *)
  !! Variable.insert "factor"  "const double" "particleCharge * stepDuration * stepDuration /particleMass / cellX" [tBefore; cVarDef "nbSteps"];
     Variable.insert "factorX" "const double" "factor / cellX" [tAfter; cVarDef "factor"];
     Variable.insert "factorY" "const double" "factor / cellY" [tAfter; cVarDef "factorX"];
     Variable.insert "factorZ" "const double" "factor / cellZ" [tAfter; cVarDef "factorY"];
  
  !! Accesses.scale (Ast.trm_var "factorX") [cVarDef "accel"; cReadVar "fieldAtPos_x"];
     Accesses.scale (Ast.trm_var "factorY") [cVarDef "accel"; cReadVar "fieldAtPos_y"];
     Accesses.scale (Ast.trm_var "factorZ") [cVarDef "accel"; cReadVar "fieldAtPos_z"];
  
  !! Accesses.scale (Ast.trm_var "stepDuration / cellX") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].speed"] ();cRead ~addr:[sExpr "(c->items)[i].speed.x"] ()];
     Accesses.scale (Ast.trm_var "stepDuration / cellY") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].speed"] ();cRead ~addr:[sExpr "(c->items)[i].speed.y"] ()];
     Accesses.scale (Ast.trm_var "stepDuration / cellZ") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].speed"] ();cRead ~addr:[sExpr "(c->items)[i].speed.z"] ()];
  
  !! Accesses.scale (Ast.trm_var "1. / cellX") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].pos"] ();cRead ~addr:[sExpr "(c->items)[i].pos.x"] ()];
     Accesses.scale (Ast.trm_var "1. / cellY") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].pos"] ();cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
     Accesses.scale (Ast.trm_var "1. / cellZ") [nbMulti;cFunDef "main"; cWrite ~lhs:[sExpr "(c->items)[i].pos"] ();cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()];
  
  (* Part: Shifting of positions*)
  !! Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_x"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_x"] ()];
     Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_y"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_y"] ()];
     Instr.inline_last_write ~write:[cWriteVar "fieldAtPos_z"] [cVarDef "accel"; cRead ~addr:[cVar "fieldAtPos_z"] ()];
     Variable.inline [nbMulti;cVarDef ~regexp:true "fieldAtPos_."];
     Variable.inline [nbMulti; cFunDef "main"; cVarDef "accel"];

  !! Variable.bind_intro ~fresh_name:"px" [sInstr "(c->items)[i].pos.x ="; dRHS];
     Variable.bind_intro ~fresh_name:"py" [sInstr "(c->items)[i].pos.y ="; dRHS];
     Variable.bind_intro ~fresh_name:"pz" [sInstr "(c->items)[i].pos.z ="; dRHS];
  !! Instr.move_multiple ~destinations:[[tAfter; cVarDef "px"];[tAfter; cVarDef "py"]] ~targets:[[cVarDef "py"];[cVarDef "pz"]];
  
  !! Variable.insert "pos2" "const vect" "{px, py, pz}" [tAfter; cVarDef "pz"];
     Accesses.shift (Ast.code "coordOfCell(idCell).ix") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.x"] ()]; [cVarDef "px"; cRead ~addr:[sExpr "(c->items)[i].pos.x"] ()]]];
     Accesses.shift (Ast.code "coordOfCell(idCell).iy") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.y"] ()]; [cVarDef "py"; cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()]]];
     Accesses.shift (Ast.code "coordOfCell(idCell).iz") [cOr [[cWrite ~lhs:[sExpr "(c->items)[i].pos.z"] ()]; [cVarDef "pz"; cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()]]];
  
)
