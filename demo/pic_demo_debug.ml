open Optitrust
open Target
open Ast 

let _ = Run.script_cpp (fun () ->

  (* Part: shifting of positions #8  *)
  !! Function.bind_args ["px"] [cFunDef "main"; tIndex 0; cFun "int_of_double"];
     Function.bind_args ["py"] [cFunDef "main"; tIndex 1; cFun "int_of_double"];
     Function.bind_args ["pz"] [cFunDef "main"; tIndex 2; cFun "int_of_double"];
     Instr.move ~target:[tAfter; cVarDef "pz"] [cFunDef "main"; cVarDef "iy"];
     Instr.move ~target:[tAfter; cVarDef "pz"] [cFunDef "main"; cVarDef "ix"];
     (* LATER: ARTHUR will figure out how to do this in one step, by allowing regexp capture in transfos. *)
     (* !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).ix") [sInstr "(c->items)[i].pos.x ="];
     !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).iy") [sInstr "(c->items)[i].pos.y ="];
     !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).iz") [sInstr "(c->items)[i].pos.z ="];
     !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).ix") [cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
     !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).iy") [cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
     !! Accesses.shift (Ast.trm_var "coordOfCell(idCell).iz") [cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()]; *)
     (* !!! (); Instead of reparsing for each transformation applied we do a single reparse at the end of shifting *)
     (* LATER
     !! Accesses.shift (Ast.trm_var "i${occ[1]}") [sInstr ~regexp:true "(c->items)\[i\].pos.\.\) ="];*)
     Accesses.shift (Ast.trm_var "ix") [sInstr "(c->items)[i].pos.x ="];
     Accesses.shift (Ast.trm_var "iy") [sInstr "(c->items)[i].pos.y ="];
     Accesses.shift (Ast.trm_var "iz") [sInstr "(c->items)[i].pos.z ="];
     Accesses.shift (Ast.trm_var "ix") [nbMulti;cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
     Accesses.shift (Ast.trm_var "iy") [nbMulti;cRead ~addr:[sExpr "(c->items)[i].pos.y"] ()];
     Accesses.shift (Ast.trm_var "iz") [nbMulti;cRead ~addr:[sExpr "(c->items)[i].pos.z"] ()];
 
  (* Part: convert pos fields to float *)
  !! Cast.insert (Ast.typ_float ()) [sInstr "(c->items)[i].pos.x ="; dRHS];
     Cast.insert (Ast.typ_float ()) [sInstr "(c->items)[i].pos.y ="; dRHS];
     Cast.insert (Ast.typ_float ()) [sInstr "(c->items)[i].pos.z ="; dRHS];

  (* Part: AOS-SOA *)
  !! Struct.inline "speed" [cTypDef "particle"];
     Struct.inline "pos" [cTypDef "particle"];


  (* Part: introduction of matrix macros *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [cVarDef "nextCharge"];

  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" [tIndex 1;cFunDef "main"; cFor "k"];
  
  !! Matrix_basic.delocalize ~dim:(trm_var "nbCorners") ~index:"i0" ~acc:"sum" [cMark "first_local"];
)
