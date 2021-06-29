open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Loop.split 1 [cFor "i"];
)
(* TODO: remove index; and remove braces *)
