open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.disable_stringreprs := true

let _ = Run.script_cpp (fun () ->
  !! ()
)
