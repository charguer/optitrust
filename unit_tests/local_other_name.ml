open ScriptTools

let _ = 
  run
    (
      fun _ -> 
      set_init_source "local_other_name.cpp";
      show_path [cFor ~name:"i"()] ~debug_ast:true;
      dump()
      
    )