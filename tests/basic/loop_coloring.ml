open Optitrust
open Run
let _ = 
    run(
        fun _ -> 
        set_init_source "loop_coloring.cpp";
        Loop.color "c" "2" [cFor "i"] ;
        Loop.color "c" "3" [cFor "j"] ;
        dump()
    )