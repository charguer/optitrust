open Optitrust
open Target

(*let _= Flags.use_new_encodings :=  false*)

let _ = Run.script_cpp (fun () ->
 (* show [sInstr "+= 2"];
 *)
  !! Instr.view_subterms ~constr:(sInstr "+= 2") [dRoot];
  !! Instr.view_subterms ~constr:(sInstr "+= 2") [cTopFunDef "main"; dBody; dSeqNth 1];



)
