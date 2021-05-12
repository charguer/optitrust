open Optitrust

let _ = 
    run(
        fun _ -> 
        set_init_source "loop_coloring.cpp";
        loop_coloring [cFor "i"] "2" "c";
        loop_coloring [cFor "j"] "3" "c";
        

        dump()
    )