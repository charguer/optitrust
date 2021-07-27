open Optitrust
open Target

(* TODO: Create a combi version of delocalize which does all these things automatically *)
let _ =  Run.script_cpp ( fun () ->
  !! Sequence.wrap ~label:"section_of_interest" ~visible:false [cFor "i"];
  !! Generic.local_other_name "T" "a"  "x" [cLabel "section_of_interest";dBody];
  !! Generic.delocalize "N" 0 "+" [cLabel "section_of_interest";dBody];
)
