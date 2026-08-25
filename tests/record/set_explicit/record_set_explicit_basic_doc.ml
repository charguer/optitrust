(* Deprecated *)
open Optitrust
open Target

let _ = Flags.typechecking_mode := Flags.ProofRepairing

let _ = Run.script_cpp (fun _ ->
  !! Record_basic.set_explicit [sInstr "a = b"];
)
