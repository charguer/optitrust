open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.fold ~at:[cVarDef "r"] [cVarDef "a"];

)
