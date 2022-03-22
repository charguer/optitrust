open Optitrust
open Target

(*let _= Flags.use_new_encodings :=  false*)

(*
let _ =
  if Str.string_match (Str.regexp "^r$") "rr" 0
  then assert false
  else assert false
  *)


let _ = Flags.dump_ast_details := true


let _ = Run.script_cpp (fun () ->


  show [cOr [[cTopFunDef "a"]; [cTopFunDef "b"]]];
  (*show [cTopFunDef "a"];
  (* !! Instr.view_subterms [dRoot]; *)
  (* !! Instr.view_subterms ~constr:(sInstr "+= 2") [dRoot]; *)
  show [sInstr "= t[0]"; dRHS];
  show [nbExact 1; sInstr "+="];
  show [sExpr "s + 1"];
  show [sExpr "s + 1"];
  show [sExpr "2"];
  show [sInstr "s + 1"];
  !! Instr.view_subterms ~constr:(sInstr "s + 1") [dRoot];
  !! Instr.view_subterms ~constr:(sInstr "+= 2") [dRoot];
  !! Instr.view_subterms ~constr:(sInstr "+= 2") [cTopFunDef "main"; dBody; dSeqNth 1];
  *)
)
