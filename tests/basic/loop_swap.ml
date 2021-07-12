open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.swap [cFor "a"];
  !! Loop.swap [cFor "a"];
)
