open ScriptTools

let _ = 
    run(
        fun _ -> 
        set_init_source "loop_tiling.cpp";
        loop_tile [cFor ~init:[cVarDef ~name:"x" ()] ()] "2" "bx";

        dump()
    )