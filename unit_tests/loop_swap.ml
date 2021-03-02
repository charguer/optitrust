open ScriptTools

let _ = 
    run(
        fun _ -> 
        set_init_source "loop_swap.cpp";
        (* TODO: CHange the name from loop transform to loop coloring*)
        loop_swap [cFor ~init:[cVarDef ~name:"a" ()] ()];
        
        dump()
    )