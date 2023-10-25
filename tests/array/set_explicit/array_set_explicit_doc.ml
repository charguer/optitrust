open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Arrays_basic.set_explicit [cVarDef "t"];

)
