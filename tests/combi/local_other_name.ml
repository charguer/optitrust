open Optitrust
open Target

(* TODO: Adapt local_other name to the new Sequence.sub functions *)
let _ = Run.script_cpp(
      fun _ -> 
      set_init_source "local_other_name.cpp";
      
      create_subsequence ~label:"sectionofinterest"  ~start:[cFor "i"] ~stop:[cVarDef "y" ()] ~stop_before:true ~braces:false ();

      local_other_name ~section_of_interest:"sectionofinterest" ~new_var:"x" ~old_var:"a" ~new_var_type:"T" ();
      
    )