open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.delete_void [cFor "i"];
)
