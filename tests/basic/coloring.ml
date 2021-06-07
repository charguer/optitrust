open Optitrust
open Run

let _ = 
    run(
        fun _ -> 
        set_init_source "coloring.cpp";
        Loop.loop_coloring [cFor "i"] "C" "ci";
        Loop.loop_coloring [cFor "j"] "C" "cj";
        dump()
    )