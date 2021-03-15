open ScriptTools

let _ = 
    run(
        fun _ -> 
        set_init_source "test_loop_transform/loop_transform.cpp";
       
        loop_tile [cFor ~init:[cVarDef ~name:"x" ()] ()] "2" "bx";
        loop_tile [cFor ~init:[cVarDef ~name:"y" ()] ()] "2" "by";
        loop_swap [cFor ~init:[cVarDef ~name:"x" ()] ()];

        loop_coloring [cFor ~init:[cVarDef ~name:"bx" ()] ()] "2" "cx";
        loop_coloring [cFor ~init:[cVarDef ~name:"by" ()] ()] "2" "cy";
        
        loop_swap [cFor ~init:[cVarDef ~name:"bx" ()] ()];
        dump()
    )