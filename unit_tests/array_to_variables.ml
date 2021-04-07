open ScriptTools

let _ = 
  run 
    ( fun _ -> 
      set_init_source"array_to_variables.cpp";
      show_path [cVarDef ~name:"y"()] ~debug_ast:true;
      
      dump()  
    )