open Optitrust

let _ = 
    run(
        fun _ -> 
        set_init_source "test_loop_transform/loop_transform.cpp";
       
        loop_tile [cFor ~init:[cVarDef "x" ] ] "2" "bx";
        loop_tile [cFor ~init:[cVarDef "y" ] ] "2" "by";
        loop_swap [cFor ~init:[cVarDef "x" ] ];

        loop_coloring [cFor ~init:[cVarDef "bx" ] ] "2" "cx";
        loop_coloring [cFor ~init:[cVarDef "by" ] ] "2" "cy";
        
        loop_swap [cFor ~init:[cVarDef "bx" ] ];
        dump()
    )