open Optitrust
open Target

(* TODO: fix *)
let _ = Run.script_cpp (fun _ ->
  !! Loop.move [cFor "y"] ~before:[cFor "cx"];
)
