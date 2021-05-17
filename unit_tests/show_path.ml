open Optitrust

let _ =
    run 
    ( fun _ ->
        set_init_source"show_path.cpp";
        
        set_repeat_io false;
        show_target [cVarDef "y"];
        clean_target_decorators();
        show_target  [cVarDef "i"] ;
        clean_target_decorators();
        show_target  [cFor "i"];
        clean_target_decorators();
        show_target  [cIf ~then_:[cVar "x++"] ()] ;
        clean_target_decorators();
        show_target  [cIf ()] ;
        clean_target_decorators();
        show_target  [cIf (); cVar "x"];
        clean_target_decorators();
        show_target  [cIf ();cVar "i"];    
        clean_target_decorators();
        show_target  [cInt 3];    
        clean_target_decorators();
        show_target  [cInstrSubstr "return"];
    
        dump()
    )