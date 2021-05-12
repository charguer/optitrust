open Optitrust

let _ = 
  run 
    ( fun _ -> 
      set_init_source"create_subsequence.cpp";
      create_subsequence ~start:[cVarDef "k"] ~stop:[cVarDef "y"] ~stop_before:false ~stop_after:true ~braces:true ();  
      dump()  
    )