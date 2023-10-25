open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.delete_void ~nest_of:2 [cFor "i"];
)
