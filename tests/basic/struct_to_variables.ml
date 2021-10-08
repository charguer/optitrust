open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Struct_basic.to_variables [cVarDef "s"];
  !! Struct_basic.to_variables [cVarDef "a"];
)
