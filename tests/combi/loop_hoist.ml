open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Loop.hoist "x_step" [cFor "i";cVarDef "x"];
  !! Loop.hoist "z_step" [cFor "j";cVarDef "y"];
  !! Loop.hoist "x1_step" [cFor "k";cVarDef "x"];
)
