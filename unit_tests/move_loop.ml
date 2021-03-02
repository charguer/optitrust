open ScriptTools

let _ = 
    run(
        fun () ->
        set_init_source "move_loop.cpp";
        move_loop_before [cFor ~init:[cVarDef ~name:"j" ()] ()] "t";
        dump ()
    )