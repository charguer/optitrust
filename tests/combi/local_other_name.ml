open Optitrust
open Target

(* TODO: Adapt local_other name to the new Sequence.sub functions *)
let _ = Run.script_cpp (fun _ -> 
      
  !! Sequence.wrap ~label:"section_of_interest" ~visible:false [cFor "i"];
  !! Generic.local_other_name "T" "a"  "x" [cLabel "section_of_interest";dBody];
      (* local_other_name ~section_of_interest:"sectionofinterest" ~new_var:"x" ~old_var:"a" ~new_var_type:"T" (); *)
      
    )