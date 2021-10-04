open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
  
  !! Sequence_basic.intro ~mark:"tofusion" 3 [cFunDef "main"; cFor "i" ~body:[sInstr "t[i]"]];
  !! Loop_basic.fusion_on_block [cMark "tofusion"];  
)
