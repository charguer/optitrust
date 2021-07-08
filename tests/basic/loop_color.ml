open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.color "C" "ci" [cFor "i"] ;
  !! Loop.color "C" "D" [cFor "j"] ;
)
