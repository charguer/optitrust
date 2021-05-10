open Optitrust

let _ =
    run 
    ( fun _ ->
        set_init_source"show_path.cpp";
        
        (* set_repeat_io false;  *)

        show_target [cVarDef ~name:"x" ()] ~debug_ast:true;
        clean_target_decorators();
        show_target  [cVarDef ~name:"i" ()] ;
        clean_target_decorators();
        show_target  [cFor ~init:[cVarDef ~name:"i" ()] ()];
        clean_target_decorators();
        show_target  [cIf ~then_:[cVar ~name:"x++" ()] ()] ;
        clean_target_decorators();
        show_target  [cIf ()] ;
        clean_target_decorators();
        show_target  [cIf (); cVar ~name:"x"()];
        clean_target_decorators();
        show_target  [cIf ();cVar ~name:"i" ()];    
        clean_target_decorators();
        show_target  [cInt 3];    
        clean_target_decorators();
        show_target  [cInstrSubstr "return"];
    
        dump()
    )