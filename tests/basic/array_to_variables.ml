open Optitrust
open Run 

let _ =
  run 
    ( fun _ -> 
      set_init_source"array_to_variables.cpp";
      show_path [cFunDef "f"];
      (* show_path [cVarDef "t" ] ~debug_ast:true; *)
      (* show_path [cTypDef "particle";cVar "t" ] ~debug_ast:true; *)
      (* show_path [cTypDef "particle"; cNth 1]: *)
      Arrays.array_to_variables [cVarDef "u" ] ["ua";"ub"];

      (* array_to_variables [cVarDef "t" ] ["ta";"tb"]; *)
      (* show_path [cVarDef "v" ] ~debug_ast:true; *)
      Arrays.array_to_variables [cVarDef "v" ] ["va";"vb"];
      dump()  
    )