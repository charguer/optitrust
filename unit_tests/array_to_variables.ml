open ScriptTools

let _ =
  run 
    ( fun _ -> 
      set_init_source"array_to_variables.cpp";
      (* show_path [cType ~name:"particle" (); cNth 1]: *)
      array_to_variables [cVarDef ~name:"t" ()] ["ta";"tb"];
      (* show_path [cVarDef ~name:"v" ()] ~debug_ast:true; *)
      array_to_variables [cVarDef ~name:"v" ()] ["va";"vb"];

      
      dump()  
    )