open Optitrust
open Target
(* Works *)
let _ = Run.script_cpp( fun _ ->
        Variable.fold [cVarDef "s1" ];
        Variable.fold [cVarDef "s2" ];
        Variable.fold [cVarDef "a" ];
    )