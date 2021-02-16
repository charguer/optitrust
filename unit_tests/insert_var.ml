open ScriptTools

let _ = 
    run
    (fun _ ->
        set_init_source"insert_var.cpp";
        let main_fun= [cFun ~name:"main" ()] in
        insert_decl ~insert_before:main_fun ~name:"size" ~value:"300" ();
          
    )