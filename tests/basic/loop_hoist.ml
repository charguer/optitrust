open Optitrust
open Target

(* Doesn't work *)
let _ = Run.script_cpp (fun () ->
    Loop.hoist "x_step" [cFor "i"];
)


