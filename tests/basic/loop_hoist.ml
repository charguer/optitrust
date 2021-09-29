open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  !! Loop_basic.hoist  [cVarDef "x"];
  !! Loop_basic.hoist  [cVarDef "z"];
)
