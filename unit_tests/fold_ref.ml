open ScriptTools

let _ = 
    run 
    ( fun _-> 
    set_init_source "fold_ref.cpp";
    fold_decl ~decl_path:[cSet ~lhs:[cVar ~name:"y" ()] ()] ();
    fold_decl ~decl_path:[cSet ~lhs:[cVar ~name:"a" ()] ()] ();
    fold_decl ~decl_path:[cSet ~lhs:[cVar ~name:"b" ()] ()] ();
    fold_decl ~decl_path:[cSet ~lhs:[cVar ~name:"v" ()] ()] ();
    dump()
)