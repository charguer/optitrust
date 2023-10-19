open Optitrust
open Target


let _ = Run.script_cpp (fun () ->

  !! Matrix.intro_malloc [cVarDef "p"];

)
