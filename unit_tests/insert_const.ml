open ScriptTools

let _ =
    run
    ( fun _ -> 
        set_init_source"insert_const.cpp";
        let main_fun= [cFun ~name:"main" ()] in
        insert_const ~insert_before:main_fun ~name:"NB_VECTS" ~value:"100" ();
        dump()
    )