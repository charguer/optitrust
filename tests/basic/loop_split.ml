open Optitrust
open Target

(* Doesn't work *)
let _ = Run.script_cpp ( fun _ ->
        Loop.split [cBefore;(cSeq ~args:[cInstr "u[i] += i"]];
)
