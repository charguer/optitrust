open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.init_detach [cVarDef "x"];
  !! Variable_basic.init_detach [cVarDef "y"];

)
