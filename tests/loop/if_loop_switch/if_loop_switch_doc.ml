open Optitrust
open Prelude
let _ = Run.script_cpp (fun _ -> Loop.if_loop_switch [ cFor "i" ])
