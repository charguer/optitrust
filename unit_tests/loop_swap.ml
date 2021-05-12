open Optitrust

let _ = 
    run(
        fun _ -> 
        set_init_source "loop_swap.cpp";
        loop_swap [cFor "a"];
        
        dump()
    )