open Optitrust
open Target

(* Works *)
let _ = Run.script_cpp (fun () ->
    Loop.hoist "x_step" [cFor "i"];
)


