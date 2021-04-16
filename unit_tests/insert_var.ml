open ScriptTools

let _ = 
    run
    (fun _ ->
        set_init_source"insert_var.cpp";
        let vect_def= [cType ~name:"vect" ()] in
        insert_decl ~insert_after:vect_def  ~name:"size" ~value:"300" ();
        dump()
    )