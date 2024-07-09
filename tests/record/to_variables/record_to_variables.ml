open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Record_basic.to_variables [cVarDef "a"];
  !! Record_basic.to_variables [cVarDef "s"];

)
