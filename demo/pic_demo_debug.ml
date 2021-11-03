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
  
  
)
