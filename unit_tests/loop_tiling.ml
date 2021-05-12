open Optitrust

let _ = 
    run(
        fun _ -> 
        set_init_source "loop_tiling.cpp";
        loop_tile [cFor "x"] "2" "bx";
        dump()
    )