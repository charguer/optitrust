open Optitrust
open Target

(* TODO: Adapt to the new syntax of Sequence.sub *)
let _ = 
  Run.script_cpp ( fun () ->
      create_subsequence ~label:"sectionofinterest"  ~start:[cFor_c"i"] ~stop:[cVarDef "y" ] ~stop_before:true ~braces:true ();
      
      local_other_name ~section_of_interest:"sectionofinterest" ~new_var:"x" ~old_var:"a" ~new_var_type:"T" ();
      
      delocalize ~section_of_interest:"sectionofinterest" ~array_size:"N" ~neutral_element:0 ~fold_operation:"+" ();
    )