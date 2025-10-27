open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.use_resources_with_models := true

let _ = Run.script_cpp (fun _ ->
  !! Variable_basic.inline [cVarDef "x"];
  !! Resources.make_strict_loop_contracts [];
)
