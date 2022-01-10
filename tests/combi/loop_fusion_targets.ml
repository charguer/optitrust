open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  
  !! Sequence_basic.intro ~mark:"tofusion" 8 [cFor "i" ~body:[sInstr "t[i]"]];
  
  !! Loop.fusion_targets [cMark "tofusion"];
)
