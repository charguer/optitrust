open Optitrust

let _ =
  run  
  (
    fun () -> 
    set_init_source"rewrite_rules.cpp";
    show_path [cVarDef ~name:"a" ()] ~debug_ast:true;
    dump()
  )