open Optitrust
open Run
let _ =
    run 
    ( fun _ ->
        set_init_source"show_path.cpp";
        
        set_repeat_io false;
        Generic.show_target [cMulti;cVar "x_step"];
        Generic.clean_target_decorators();
        Generic.show_target  [cVarDef "i"] ;
        Generic.clean_target_decorators();
        Generic.show_target  [cFor "i"];
        Generic.clean_target_decorators();
        Generic.show_target  [cIf ~then_:[cVar "x++"] ()] ;
        Generic.clean_target_decorators();
        Generic.show_target  [cIf ()] ;
        Generic.clean_target_decorators();
        Generic.show_target  [cIf (); cVar "x"];
        Generic.clean_target_decorators();
        Generic.show_target  [cIf ();cVar "i"];    
        Generic.clean_target_decorators();
        Generic.show_target  [cInt 3];    
        Generic.clean_target_decorators();
        Generic.show_target  [cInstr "return"];
    
        dump()
    )