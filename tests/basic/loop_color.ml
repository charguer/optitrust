open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.color "C" ~index:"ci" [cFor "i"];
  !! Loop_basic.color "C" [cFor "j"];
  (* LATER: in the future, we will check that "ci" is not conflicting
     with an existing variable *)
)
