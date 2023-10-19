open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.to_nonconst [cVarDef "x"];

)
