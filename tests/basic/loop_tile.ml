open Optitrust
open Run
let _ = 
    run(
        fun _ -> 
        set_init_source "loop_tiling.cpp";
        Loop.tile "2" "bx" [cFor "x"] ;
        dump()
    )