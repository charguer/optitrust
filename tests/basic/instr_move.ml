open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Instr_basic.move ~dest:[tBefore;cVarDef "x"] [cVarDef "z"];
  !! Instr_basic.move ~dest:[tBefore;cVarDef "y"] [cVarDef "x"]; 
  !! Instr_basic.move ~dest:[tAfter;cVarDef "z"] [cVarDef "x"];
  !! Instr_basic.move ~dest:[tAfter;cVarDef "y"] [cVarDef "x"];
  !! Instr_basic.move ~dest:[tAfter;cVarDef "x"] [cVarDef "z"];
  !! Instr_basic.move ~dest:[tAfter;cVarDef "x"] [cVarDef "y"];
)
