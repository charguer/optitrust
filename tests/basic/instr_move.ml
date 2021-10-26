open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Instr_basic.move ~target:[tBefore;cVarDef "x"] [cVarDef "z"];
  !! Instr_basic.move ~target:[tBefore;cVarDef "y"] [cVarDef "x"]; 
  !! Instr_basic.move ~target:[tAfter;cVarDef "z"] [cVarDef "x"];
  !! Instr_basic.move ~target:[tAfter;cVarDef "y"] [cVarDef "x"];
  !! Instr_basic.move ~target:[tAfter;cVarDef "x"] [cVarDef "z"];
  !! Instr_basic.move ~target:[tAfter;cVarDef "x"] [cVarDef "y"];
)
