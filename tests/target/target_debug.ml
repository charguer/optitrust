open Optitrust
open Prelude
let show = Show.add_marks_for_target_unit_tests

(*let _= Flags.use_new_encodings :=  false*)

(*
let _ =
  if Str.string_match (Str.regexp "^r$") "rr" 0
  then assert false
  else assert false
  *)


let _ = Flags.dump_ast_details := true


let _ = Run.script_cpp (fun () ->

  !! show [cOr [[cVarDef "a"];[cVarDef "b"];[cVarDef "c"];[cVarDef "d"]]];
  !! show [cVarDefs ["a";"b";"c";"d"]];

  (*show [cTopFunDef "a"];
  (* !! Expr.view_subterms [dRoot]; *)
  (* !! Expr.view_subterms ~constr:(sInstr "+= 2") [dRoot]; *)
  show [sInstr "= t[0]"; dRHS];
  show [nbExact 1; sInstr "+="];
  show [sExpr "s + 1"];
  show [sExpr "s + 1"];
  show [sExpr "2"];
  show [sInstr "s + 1"];
  !! Expr.view_subterms ~constr:(sInstr "s + 1") [dRoot];
  !! Expr.view_subterms ~constr:(sInstr "+= 2") [dRoot];
  !! Expr.view_subterms ~constr:(sInstr "+= 2") [cTopFunDef "main"; dBody; dSeqNth 1];
  *)
)
