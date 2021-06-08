open Optitrust
open Run

(* Works *)
let _ = run_unit_test ( fun _ -> 
        Loop.swap [cFor "a"];
     )