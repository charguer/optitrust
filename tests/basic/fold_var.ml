open Optitrust
open Run
let _ = 
    run
    ( fun _ ->
        set_init_source"fold_var.cpp";
        Declaration.fold [cVarDef "s1" ] ();
        Declaration.fold [cVarDef "s2" ] ();
        Declaration.fold [cVarDef "a" ] ();
        dump ()

    )