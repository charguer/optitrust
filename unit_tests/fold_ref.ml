open Optitrust
open Run
let _ = 
    run 
    ( fun _-> 
    set_init_source "fold_ref.cpp";
    Declaration.fold [cSet ~lhs:[cVar ~name:"y" ()] ()] ();
    Declaration.fold [cSet ~lhs:[cVar ~name:"a" ()] ()] ();
    Declaration.fold [cSet ~lhs:[cVar ~name:"b" ()] ()] ();
    Declaration.fold [cSet ~lhs:[cVar ~name:"v" ()] ()] ();
    dump()
)