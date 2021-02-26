open ScriptTools

let _ = 
    run(
        fun _ -> 
        set_init_source "loop_transform.cpp";
        (*loop_swap [cFor ~init:[cVarDef ~name:"a" ()] ()];
         *)
        loop_swap [cFor ~init:[cVarDef ~name:"a" ()] ()];
        loop_tile [cFor ~init:[cVarDef ~name:"x" ()] ()] "2";
        loop_tile [cFor ~init:[cVarDef ~name:"y" ()] ()] "2";
        
        loop_transform [cFor ~init:[cVarDef ~name:"bx" ()] ()] "2" ;
        loop_transform [cFor ~init:[cVarDef ~name:"by" ()] ()] "2" ;
        
        
        dump()
    )