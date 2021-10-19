open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.replace_occurrences "y" ~space:"(2 + x)" [cVarDef "z"]; (* TODO: rename to    subst "a" ~put:"x" tg *)
  !! Variable_basic.replace_occurrences "y" ~space:"5" [cFunDef "main"];
)
