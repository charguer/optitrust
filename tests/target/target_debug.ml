open Optitrust
open Target

(*let _= Flags.use_new_encodings :=  false*)

let _ = Run.script_cpp (fun () ->

  (* TODO: create a unit test *)
  (*!! Instr.view_subterms [dRoot];*)
  show [sInstr "+="];
  show [sExpr "s + 1"];
  show [sExpr "s + 1"];
  show [sExpr "2"];
  show [sInstr "s + 1"];
  (*!! Instr.view_subterms ~constr:(sInstr "s + 1") [dRoot];*)
  (* !! Instr.view_subterms ~constr:(sInstr "+= 2") [dRoot]; TODO: test this line after += support is fixed *)
  (*!! Instr.view_subterms ~constr:(sInstr "+= 2") [cTopFunDef "main"; dBody; dSeqNth 1]; *)



)
