open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
  (*!! Instr.gather_targets ~dest:(GatherAt [tBefore; occLast; cFor "i"]) [nbMulti; cFor "i"];*)
  !! Loop.fusion_targets ~nest_of:2 ~into:(FuseInto [occLast; cFor "i"]) [nbMulti; cFor "i"];
)
