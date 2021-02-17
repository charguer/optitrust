open ScriptTools

let _ =
    run
    ( fun _ -> 
        set_init_source"insert_const.cpp";
        let vect_def= [cType ~name:"vect" ()] in
        insert_const ~insert_after:vect_def ~name:"NB_VECTS" ~value:"100" ();
        dump()
    )