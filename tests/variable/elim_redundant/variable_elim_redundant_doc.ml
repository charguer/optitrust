open Optitrust
open Target



let _ = Run.script_cpp (fun () ->

  !! Variable.elim_redundant [cVarDef "b"];

)
