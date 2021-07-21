open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.to_unit_steps ~new_index:"j" [cFor "i"];
)
