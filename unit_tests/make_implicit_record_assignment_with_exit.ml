open Optitrust



let _ = 
    run 
    ( fun _ -> 
        set_init_source"make_implicit_record_assignment.cpp";
        (* detach_expression [cVarDef ~name:"b"()] ~keep_label:false; *)
(        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect"; exit_script ());

        make_implicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect" ; 
        
        make_implicit_record_assignment [cVarDef ~name:"d"()] ~struct_name:"vect" ; 
        
        detach_expression [cVarDef ~name:"b"()] ~keep_label:true; 
        make_explicit_record_assignment [cLabel ~label:"detached"();cBody()] ~struct_name:"vect";
        
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect";
        
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect";
        delete_label "detached";

        dump()
    )
