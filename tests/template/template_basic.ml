open Optitrust
open Prelude

let _ = Flags.typechecking_mode := Flags.ProofRepairing
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () -> ());
