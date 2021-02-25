open ScriptTools

let _ = 
    run(
        fun _ -> 
        set_init_source "coloring.cpp";
        loop_transform [cFor ~init:[cVarDef ~name:"i" ()] ()] "C" ;
        dump()
    )