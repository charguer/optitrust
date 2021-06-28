open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
    !! Loop.hoist "x_step" [cForSimple "i"];
)
