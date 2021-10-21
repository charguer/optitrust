open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Instr_basic.move ~where:[tBefore;cVarDef "x"] [cVarDef "z"];
  !! Instr_basic.move ~where:[tBefore;cVarDef "y"] [cVarDef "x"]; 
  !! Instr_basic.move ~where:[tAfter;cVarDef "z"] [cVarDef "x"];
  !! Instr_basic.move ~where:[tAfter;cVarDef "y"] [cVarDef "x"];
  !! Instr_basic.move ~where:[tAfter;cVarDef "x"] [cVarDef "z"];
  !! Instr_basic.move ~where:[tAfter;cVarDef "x"] [cVarDef "y"];
)
