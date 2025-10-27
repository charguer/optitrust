open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags. use_resources_with_models := true

let _ = Run.script_cpp (fun _ ->
  !! Arith_basic.(simpl_rec gather_rec) [cFunBody "test_model"; cVarDef "b"];
)
