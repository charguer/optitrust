open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  !! Loop_basic.fusion_on_block [cLabel "tofusion"];
)

