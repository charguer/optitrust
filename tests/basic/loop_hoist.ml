open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  !! Loop_basic.hoist "x_step" [cVarDef "x"];
  !! Loop_basic.hoist "z_step" [cVarDef "z"];
)
