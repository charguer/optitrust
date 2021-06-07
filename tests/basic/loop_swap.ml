open Optitrust
open Run

let _ = 
    run(
        fun _ -> 
        set_init_source "loop_swap.cpp";
        Loop.swap [cFor "a"];
        dump()
    )