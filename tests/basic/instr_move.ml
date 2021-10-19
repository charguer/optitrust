open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Instr.move ~before:[cVarDef "x"] [cVarDef "z"];

  !! Instr.move ~before:[cVarDef "y"] [cVarDef "x"]; (* TODO: fix should do nothing if already at the right place *)

  !! Instr.move ~after:[cVarDef "z"] [cVarDef "x"];
  !! Instr.move ~after:[cVarDef "y"] [cVarDef "x"];
  !! Instr.move ~after:[cVarDef "x"] [cVarDef "z"];
  !! Instr.move ~after:[cVarDef "x"] [cVarDef "y"];
)
