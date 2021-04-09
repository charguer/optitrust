open ScriptTools

let _ = 
  run
    (
      fun _ -> 
      set_init_source "local_other_name.cpp";
      create_subsequence ~label:"sectionofinterest"  [cFor ~name:"i" ()] [cVarDef ~name:"y" ()];
      show_path [cFor ~name:"i"()] ~debug_ast:true;

      local_other_name ~section_of_interest:"sectionofinterest" ~new_var:"x" ~new_var_type:"T" ();
      dump()
      
    )