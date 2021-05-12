open Optitrust

let _ = 
    run(
        fun _ -> 
        set_init_source "test_loop_transform/loop_transform.cpp";
       
        loop_tile [cFor "x"] "2" "bx";
        loop_tile [cFor "y"] "2" "by";
        loop_swap [cFor "x"];

        loop_coloring [cFor "bx"] "2" "cx";
        loop_coloring [cFor "by"] "2" "cy";
        
        loop_swap [cFor "bx"];
        dump()
    )