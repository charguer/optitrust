open Optitrust

let _ = 
    run(
        fun _ -> 
        set_init_source "loop_coloring.cpp";
        loop_coloring [cFor ~init:[cVarDef ~name:"i" ()] ()] "2" "c";
        loop_coloring [cFor ~init:[cVarDef ~name:"j" ()] ()] "3" "c";
        

        dump()
    )