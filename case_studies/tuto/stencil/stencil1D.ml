open Optitrust
open Prelude
let _ = Flags.check_validity := false

let _ = Run.script_cpp (fun _ ->
  !! Loop.fission [cForBody "i"; tBefore; dSeqNth 1];)
