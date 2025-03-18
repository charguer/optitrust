open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! Resources.detach_loop_ro_focus [cFor "i"];
)

