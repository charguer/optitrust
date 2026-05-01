open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.use_resources_with_models := true

let _ = Run.script_cpp (fun () ->
  !! Resources.ensure_computed ();
  !! Loop.hoist ~nest_of:2 ~inline:false [nbMulti; cFor "z_i"; cVarDefs ["z_a"; "z_b"]];

  !! Resources.make_strict_loop_contracts [];
)
