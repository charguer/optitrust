open Optitrust
open Target

(*let _= Flags.use_new_encodings :=  false*)

(*
let _ =
  if Str.string_match (Str.regexp "^r$") "rr" 0
  then assert false
  else assert false
  *)

let _ = Run.script_cpp (fun () ->

  (*!! Instr.view_subterms ~constr:(sInstr "t[0]") [dRoot];*)
  show [tBefore; sInstr "t[0]"];
  (*
  (* !! Instr.view_subterms [dRoot]; *)
  (* !! Instr.view_subterms ~constr:(sInstr "+= 2") [dRoot]; *)
<<<<<<< HEAD
  show [sInstr "= t[0]"; dRHS];
=======
  show [nbExact 1; sInstr "+= 2"];
>>>>>>> 0c090a1eaf597711941d96130278402d492644e5
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
