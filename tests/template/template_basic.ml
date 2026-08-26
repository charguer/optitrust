open Optitrust
open Prelude

let _ = Flags.typechecking_mode := Flags.ProofPreserving
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () -> ());
