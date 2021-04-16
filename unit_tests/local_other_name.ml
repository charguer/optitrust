open ScriptTools

let _ = 
  run
    (
      fun _ -> 
      set_init_source "local_other_name.cpp";
      
      create_subsequence ~label:"sectionofinterest"  ~start:[cFor ~name:"i" ()] ~stop:[cVarDef ~name:"y" ()] ~stop_before:true ~braces:false ();

      local_other_name ~section_of_interest:"sectionofinterest" ~new_var:"x" ~old_var:"a" ~new_var_type:"T" ();
      dump()
      
    )