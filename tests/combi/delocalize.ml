open Optitrust
open Run

(* TODO: Adapt to the new syntax of Sequence.sub *)
let _ = 
  run_unit_test ( fun () ->
      create_subsequence ~label:"sectionofinterest"  ~start:[cFor "i"] ~stop:[cVarDef "y" ] ~stop_before:true ~braces:true ();
      (* show_path [cLabel "sectionofinterest";cBody ()] ~debug_ast:true; *)
      
      local_other_name ~section_of_interest:"sectionofinterest" ~new_var:"x" ~old_var:"a" ~new_var_type:"T" ();
      (* show_path [cLabel "sectionofinterest";cBody ()] ~debug_ast:true; *)
      
      (* Printf.printf "outercalltodelocalize\n"; *)
      delocalize ~section_of_interest:"sectionofinterest" ~array_size:"N" ~neutral_element:0 ~fold_operation:"+" ();
    )