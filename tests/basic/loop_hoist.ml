open Optitrust
open Run

(* Doesn't work *)
let _ = run_unit_test (fun () ->
    Loop.hoist "x_step" [cFor "i"];
)


