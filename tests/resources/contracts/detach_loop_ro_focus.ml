open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
  !! Resources.detach_loop_ro_focus [cFor "i"];
)

