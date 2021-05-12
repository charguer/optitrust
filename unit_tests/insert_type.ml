open Optitrust

let _ =
    run
    (fun _ ->
        set_init_source"insert_type.cpp";
        let main_fun= [cFunDef "main"] in
        insert_typedef ~insert_before:main_fun ~name:"T" ~value:"T[M][N]" ();
        
        dump()
    ) 