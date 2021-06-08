open Optitrust
open Run

(* Works *)
let _ = run_unit_test (fun _ -> 
        Loop.tile "2" "bx" [cFor "x"] ;
    )