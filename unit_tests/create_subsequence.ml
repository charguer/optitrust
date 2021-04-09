open ScriptTools

let _ = 
  run 
    ( fun _ -> 
      set_init_source"create_subsequence.cpp";
      create_subsequence [cVarDef ~name:"x"()] [cVarDef ~name:"y"()] ~braces:true;  
      dump()  
    )