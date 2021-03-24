open ScriptTools



let _ = 
    run 
    ( fun _ -> 
        set_init_source"make_implicit_record_assignment.cpp";
        detach_expression [cVarDef ~name:"b"()] ~keep_label:true ~keep_braces:false;
        make_explicit_record_assignment [cLabel ~label:"detached"();cBody()] ~struct_name:"vect";
     
        make_implicit_record_assignment ~struct_name:"vect" (); 
        dump()
    )
