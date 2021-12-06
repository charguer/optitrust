open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.to_const [cVarDef "x"];
  !! Variable_basic.to_const [cTopFunDef "main";cVarDef "y"];
  !! Variable_basic.to_const [cVarDef "z"];
)
