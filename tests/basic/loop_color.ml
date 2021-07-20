open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.color "C" "ci" [cFor "i"];
  !! Loop.color "C" "D" [cFor "j"];
  (* TODO:
        Loop.color "C" ~index:"ci" [cFor "i"] ;
        Loop.color "C" [cFor "j"] ;
     default value for index is "ci" when the loop has index "i"  *)
  (* LATER: in the future, we will check that "ci" is not conflicting
     with an existing variable *)
)
