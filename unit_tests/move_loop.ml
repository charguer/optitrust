open ScriptTools

let _ = 
    run(
        fun () ->
        set_init_source "move_loop.cpp";
        move_loop_after [cFor ~init:[cVarDef ~name:"i" ()] ()] "u";
        move_loop_before [cFor ~init:[cVarDef ~name:"j" ()] ()] "u";
        
        dump ()
    )