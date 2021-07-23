open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.grid_enumerate [("x", "X"); ("y", "Y"); ("z", "Z")] [cFor "idCell"];
)
