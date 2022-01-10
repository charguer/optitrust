open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable.inline_and_rename [cVarDef "y"];
)
