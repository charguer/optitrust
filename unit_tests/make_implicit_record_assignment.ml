open Optitrust



let _ = 
    run 
    ( fun _ -> 
        set_init_source"make_implicit_record_assignment.cpp";
        (* detach_expression [cVarDef "b"] ~keep_label:false;  *)

        make_explicit_record_assignment [cVarDef "b"] ~struct_name:"vect";

        make_implicit_record_assignment [cVarDef "b"] ~struct_name:"vect" ; 
        
        make_implicit_record_assignment [cVarDef "d"] ~struct_name:"vect" ; 
        
        detach_expression [cVarDef "b"] ~keep_label:true; 
        make_explicit_record_assignment [cLabel "detached";cBody()] ~struct_name:"vect";
        
        make_explicit_record_assignment [cVarDef "b"] ~struct_name:"vect";
        
        make_explicit_record_assignment [cVarDef "b"] ~struct_name:"vect";
        delete_label "detached";

        dump()
    )
