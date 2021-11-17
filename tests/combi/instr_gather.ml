open Optitrust
open Target

(* TODO: Fix the issue with GatherAtLast *)
let _ = Run.script_cpp (fun _ ->

  
  (* !! Instr.(gather ~dest:GatherAtFirst) [cVarDef ""]; *)
  !! Instr_basic.move ~dest:[tAfter; occLast;cVarDef ""] [nbMulti;cVarDef ""];
  !! Instr.(gather ~dest:GatherAtLast) [cVarDef ""];
  !! Instr.(gather ~dest:(GatherAt [tBefore;cVarDef "y"])) [cVarDef ""];
)
