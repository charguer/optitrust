open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.move1 [cFor "bx"] ~before:[cFor "y"];
  (* !! Loop.move "x" ~before:"cx"; *)
)
