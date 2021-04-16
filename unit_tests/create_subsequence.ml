open ScriptTools

let _ = 
  run 
    ( fun _ -> 
      set_init_source"create_subsequence.cpp";
      create_subsequence ~start:[cVarDef ~name:"k"()] ~stop:[cVarDef ~name:"y"()] ~stop_before:false ~stop_after:true ~braces:true ();  
      dump()  
    )