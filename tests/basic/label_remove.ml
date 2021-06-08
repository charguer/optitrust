open Optitrust
open Run

(* Works *)
let _ = run_unit_test ( fun _ -> 
      Label.remove [cLabel "start"];
      Label.remove_multiple [[cLabel "loop"]; [cLabel "cond"]; [cLabel"incr_1"];[cLabel "incr_2"];[cLabel "stop"]];
   )