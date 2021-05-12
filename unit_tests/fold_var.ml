open Optitrust

let _ = 
    run
    ( fun _ ->
        set_init_source"fold_var.cpp";
        fold_decl ~decl_path:[cVarDef "s1" ] ();
        fold_decl ~decl_path:[cVarDef "s2" ] ();
        fold_decl ~decl_path:[cVarDef "a" ] ();
        dump ()

    )