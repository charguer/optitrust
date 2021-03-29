open ScriptTools



let _ = 
    run 
    ( fun _ -> 
        set_init_source"make_implicit_record_assignment.cpp";
        (* detach_expression [cVarDef ~name:"b"()] ~keep_label:false; *)
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect";
        show_path [cVarDef ~name:"e"()] ~debug_ast:true;
 
        make_implicit_record_assignment ~struct_name:"vect" "b"; 
        
        detach_expression [cVarDef ~name:"b"()] ~keep_label:true; 
        make_explicit_record_assignment [cLabel ~label:"detached"();cBody()] ~struct_name:"vect";
        
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect";
        
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect";
        delete_label "detached";
        
        dump()
    )
