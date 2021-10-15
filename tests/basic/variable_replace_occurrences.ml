open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  !! Variable_basic.replace_occurrences "y" ~space:"x" [cVarDef "z"];
  !! Variable_basic.replace_occurrences "y" ~space:"5" [cFunDef "main"];
)
