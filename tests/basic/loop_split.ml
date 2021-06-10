open Optitrust
open Run

(* Doesn't work *)
let _ = run_unit_test ( fun _ ->
        Loop.split [cBefore; cSeq ~args:[cInstr "u[i] += i"]];
)
