open Optitrust
open Run
(* Does not work *)
let _ = run_unit_test( fun _ ->
        Declaration.fold [cVarDef "s1" ];
        Declaration.fold [cVarDef "s2" ];
        Declaration.fold [cVarDef "a" ];
    )