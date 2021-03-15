open ScriptTools



let _ = 
    run 
    ( fun _ -> 
        set_init_source"make_explicit_assigment.cpp";
        make_explicit_record_assignment "vect";
        dump()
    )
