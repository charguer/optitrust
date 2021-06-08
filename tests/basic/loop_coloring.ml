open Optitrust
open Run

(*  *)
let _ = run_unit_test (fun _ ->
        Loop.color "C" "2" [cFor "i"] ;
        Loop.color "C" "D" [cFor "j"] ;
)