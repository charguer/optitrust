open Optitrust
open Target


let _ = Run.script_cpp (fun () ->

  !! Variable.inline_and_rename [cVarDef "y"];

)
