open Optitrust
open Target

(*let _= Flags.use_new_encodings :=  false*)

let _ = Run.script_cpp (fun () ->
 (* show [sInstr "+= 2"];
 *)
  (* TODO: create a unit test *)
  !! Instr.view_subterms [dRoot];
  !! Instr.view_subterms ~constr:(sInstr "+= 2") [dRoot];
  !! Instr.view_subterms ~constr:(sInstr "+= 2") [cTopFunDef "main"; dBody; dSeqNth 1];



)
