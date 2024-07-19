open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->
  !! Record_basic.to_variables [cVarDef "a"];
  !! Record_basic.to_variables [cVarDef "b"];
  !! Record_basic.to_variables [cVarDef "s"];
)
