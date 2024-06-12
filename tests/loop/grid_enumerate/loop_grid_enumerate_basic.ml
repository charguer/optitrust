open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.grid_enumerate [("x", expr "X"); ("y", expr "Y"); ("z", expr "Z")] [cFor "idCell"];

  !!! (); (* TODO: Find how to eliminate this reparse *)
)
