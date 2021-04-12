open ScriptTools

let _ =
  run 
    ( fun _ -> 
      set_init_source"array_to_variables.cpp";
      show_path [cFun ~name:"f"()];
      (* show_path [cVarDef ~name:"t" ()] ~debug_ast:true; *)
      (* show_path [cType ~name:"particle"();cVar ~name:"t" ()] ~debug_ast:true; *)
      (* show_path [cType ~name:"particle" (); cNth 1]: *)
      array_to_variables [cVarDef ~name:"u" ()] ["ua";"ub"];

      (* array_to_variables [cVarDef ~name:"t" ()] ["ta";"tb"]; *)
      (* show_path [cVarDef ~name:"v" ()] ~debug_ast:true; *)
      array_to_variables [cVarDef ~name:"v" ()] ["va";"vb"];

      
      dump()  
    )