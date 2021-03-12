open ScriptTools

let _ = 
    run(
        fun () ->
        set_init_source "move_loop.cpp";
        move_loop "c" ~move_after:"d";
        move_loop "c" ~move_before:"d";
        
        dump ()
    )