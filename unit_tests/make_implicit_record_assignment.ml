open ScriptTools



let _ = 
    run 
    ( fun _ -> 
        set_init_source"make_implicit_record_assignment.cpp";
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect";
        make_implicit_record_assignment ~struct_name:"vect" "b"; 
        
        detach_expression [cVarDef ~name:"b"()] ~keep_label:true; 
        make_explicit_record_assignment [cLabel ~label:"detached"();cBody()] ~struct_name:"vect";
        
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect";
        
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect";
        delete_label "detached";
        (*detach_expression [cVarDef ~name:"b"()] ~keep_label:true ~keep_braces:false;
        make_explicit_record_assignment [cLabel ~label:"detached"();cBody()] ~struct_name:"vect";
     
        TODO: Find out why calling first deteach expression than make_explicit_record_assignment is not working anymore*)
        
        dump()
    )
