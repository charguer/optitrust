open ScriptTools

let _ = 
    run(
        fun _ -> 
        set_init_source "loop_transform.cpp";
        (* TODO: CHange the name from loop transform to loop coloring*)
        loop_swap [cFor ~init:[cVarDef ~name:"a" ()] ()];
        loop_transform [cFor ~init:[cVarDef ~name:"k" ()] ()] "3" "ck";
        loop_transform [cFor ~init:[cVarDef ~name:"x" ()] ()] "2" "cx";

        (* TODO; use a separate file to find out the encoding for x += 3 *)
        loop_tile [cFor ~init:[cVarDef ~name:"x" ()] ()] "2" "bx";
        loop_tile [cFor ~init:[cVarDef ~name:"y" ()] ()] "2" "by";
        
        loop_transform [cFor ~init:[cVarDef ~name:"bx" ()] ()] "2" "cx";
        loop_transform [cFor ~init:[cVarDef ~name:"by" ()] ()] "2" "cy";

        
        
        
        dump()
    )