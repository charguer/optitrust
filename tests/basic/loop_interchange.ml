open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.interchange [cFor "a"];
  !! Loop_basic.interchange [cFor "a"];
  !! Loop_basic.interchange [cFor "c"];
  !! Loop_basic.interchange [cFor "b"];
)
