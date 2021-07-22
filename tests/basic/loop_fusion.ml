open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->

  !! Loop.fusion_on_block [cLabel "tofusion"];
)
