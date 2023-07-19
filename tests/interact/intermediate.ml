open Optitrust
open Target
open Syntax

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.insert (stmt "int b = 2;") [tAfter; cVarDef "a"];
  !! Sequence_basic.insert (stmt "int c = 3;") [tAfter; cVarDef "a"];
  !! Sequence_basic.insert (stmt "int d = 4;") [tAfter; cVarDef "a"]; (* click on this line then use shortcut to save intermediate state *)
  !! Sequence_basic.insert (stmt "int e = 5;") [tAfter; cVarDef "a"]; (* click on this line then use shortcut to view diff using intermediate state *)
)
