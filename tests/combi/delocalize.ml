open Optitrust
open Target

let _ =  Run.script_cpp ( fun () ->
  !! Generic.delocalize ~var_type:"T" ~old_var:"a" ~new_var:"x" ~label:"section_of_interest" ~arr_size:"N" ~neutral_element:0 "+" [cFor "i"];
  !! Trace.alternative (fun () ->
    !! Sequence_basic.intro_on_instr ~label:"section_of_interest" ~visible:false [cFor "i"];
    !! Generic_basic.local_other_name "T" "a"  "x" [cLabel "section_of_interest";dBody]; (* TODO: move to Variable *)
    !! Generic_basic.delocalize "N" 0 "+" [cLabel "section_of_interest";dBody];  
    !! ();)
)
