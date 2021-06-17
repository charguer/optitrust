open Optitrust
open Target

(* Doesn't work *)
let _ = Run.script_cpp ( fun _ ->
        show [cBefore;cSeq ~args:[cInstr "u[i] += i"]()];
        Loop.split [cBefore;cSeq ~args:[cInstr "u[i] += i"]()];
)
