open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  
  !! Instr.(gather_targets ~dest:(GatherAt [tBefore;cVarDef "x"])) [cVarDef ""];
  !! Instr.(gather_targets ~dest:GatherAtFirst) [cVarDef ""];
  !! Instr_basic.move ~dest:[tAfter; occLast;cVarDef ""] [nbMulti;cVarDef ""];
  !! Instr.(gather_targets ~dest:GatherAtLast) [cVarDef ""];
  !! Instr.(gather_targets ~dest:(GatherAt [tBefore;cVarDef "y"])) [cVarDef ""];
)
