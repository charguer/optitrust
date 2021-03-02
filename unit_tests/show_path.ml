open ScriptTools

let _ =
    run 
    ( fun _ ->
        set_init_source"show_path.cpp";
        show_path  [cVarDef ~name:"x" ()] ;
        clean_path_decorators();
        show_path  [cVarDef ~name:"i" ()] ;
        clean_path_decorators();
        show_path  [cFor ~init:[cVarDef ~name:"i" ()] ()];
        clean_path_decorators();
        show_path  [cIf ~then_:[cVar ~name:"x++" ()] ()] ;
        clean_path_decorators();
        show_path  [cIf ()] ;
        clean_path_decorators();
        show_path  [cIf (); cVar ~name:"x"()];
        clean_path_decorators();
        show_path  [cIf ();cVar ~name:"i" ()];    
        clean_path_decorators();
        show_path  [cInt 3];    
        clean_path_decorators();
        show_path  [cInstrSubstr "return"];
    
        dump()
    )