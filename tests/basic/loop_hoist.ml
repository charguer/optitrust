open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Loop.hoist "x_step" [cVarDef "x"];
  !! Loop.hoist "z_step" [cVarDef "z"];
)
