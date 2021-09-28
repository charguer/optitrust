open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

(* TODO: Fix me! *)

  !! Loop.reorder ["y";"x";"z"][cFor "x"];
  !! Loop.reorder ["x";"y";"z"][cFor "y"];
  
)