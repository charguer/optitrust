open Optitrust
open Run
(* TODO: Not yet implemented*)
let _ =
  run  
  (
    fun () -> 
    set_init_source"rewrite_rules.cpp";
    show_path [cVarDef "a"] ~debug_ast:true;
    dump()
  )