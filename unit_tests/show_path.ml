open ScriptTools

let _ =
    run 
    ( fun _ ->
        set_init_source"show_path.cpp";
        show_path  [cVarDef ~name:"x" ()] ;
        show_path  [cFor ~init:[cVarDef ~name:"i" ()] ()];
        show_path  [cIf ~then_:[cVar ~name:"x++" ()] ()] ;
        show_path  [cIf (); cVar ~name:"x"()];
        show_path  [cIf ();cVar ~name:"i" ()];    
        show_path  [cInstrSubstr "return"];
    
        dump()
    )