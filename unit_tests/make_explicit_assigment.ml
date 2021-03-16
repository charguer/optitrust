open ScriptTools



let _ = 
    run 
    ( fun _ -> 
        set_init_source"make_explicit_assigment.cpp";
        (*make_explicit_record_assigment "vect" *)
        (*
        make_explicit_record_assignment [cSet ~lhs:[cAccesses ~accesses:[cField ~field:"pos"()]  ()] ~rhs:[cVar ~name:"pos"()]()] ~struct_name:"vect";
        *)
        make_explicit_record_assignment [cVarDef ~name:"b" ()] ~struct_name:"vect";
        show_path [cApp ~name:"overloaded=" ~args:[cVar ~name:"b" (); cVar ~name:"p" ()] ()];
        make_explicit_record_assignment [cApp ~name:"overloaded=" ~args:[cVar ~name:"b" (); cVar ~name:"p" ()] ()] ~struct_name:"vect";
        dump()
    )
