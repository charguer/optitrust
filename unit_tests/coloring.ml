open Optitrust

let _ = 
    run(
        fun _ -> 
        set_init_source "coloring.cpp";
        loop_coloring [cFor "i"] "C" "ci";
        loop_coloring [cFor "j"] "C" "cj";
        dump()
    )