open Optitrust
open Target

let _ = Run.script_cpp( fun _ ->
        Variable.insert_and_fold "s1" "x * y" [tBefore;cVarDef "r1"];
        Variable.insert_and_fold "s2" "y * x" [tBefore;cVarDef "r2"];
    )