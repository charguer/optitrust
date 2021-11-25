open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->

  (*!! Sequence_basic.intro ~mark:"tofuse" 3 [cFunDef "main"; cFor "i" ~body:[sInstr "t[i]"]];*)

  !! Loop_basic.fusion_on_block [cLabel "tofuse"];
)
