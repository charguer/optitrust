open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.interchange [cFor "a"];
  !! Loop.interchange [cFor "a"];
  !! Loop.interchange [cFor "c"];
  !! Loop.interchange [cFor "b"];
)
