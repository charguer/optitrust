open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.tile "2" ~tile_index:"bx" [cFor "x"];
  !! Loop.tile "2"  [cFor "y"];
)
