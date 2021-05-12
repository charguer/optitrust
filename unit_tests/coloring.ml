open Optitrust

let _ = 
    run(
        fun _ -> 
        set_init_source "coloring.cpp";
        loop_coloring [cFor ~init:[cVarDef "i" ] ()] "C" "ci";
        loop_coloring [cFor ~init:[cVarDef "j" ] ()] "C" "cj";
        dump()
    )