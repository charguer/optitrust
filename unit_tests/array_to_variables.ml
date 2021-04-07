open ScriptTools

let _ = 
  run 
    ( fun _ -> 
      set_init_source"array_to_variables.cpp";
      (* show_path [cVarDef ~name:"t"()] ~debug_ast:true; *)
      array_to_variables [cVarDef ~name:"t" ()] ["ta";"tb"];
      
      dump()  
    )