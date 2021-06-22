open Optitrust
open Target
(* Works *)
let _ = Run.script_cpp( fun _ ->
        Declaration.fold [cVarDef "s1" ];
        Declaration.fold [cVarDef "s2" ];
        Declaration.fold [cVarDef "a" ];
    )