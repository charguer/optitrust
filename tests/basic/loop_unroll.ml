open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.unroll [cFor "i"];
)

