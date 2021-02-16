open ScriptTools

let _ = 
    run
    ( fun _ ->
        set_init_source"fold_var.cpp";
        fold_decl ~decl_path:[cVarDef ~name:"s1" ()] ();
        fold_decl ~decl_path:[cVarDef ~name:"s2" ()] ();
        fold_decl ~decl_path:[cVarDef ~name:"a" ()] ();
        dump ()

    )