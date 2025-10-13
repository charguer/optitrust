open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.use_resources_with_models := true
let _ = Flags.preserve_specs_only := true

let _ = Run.script_cpp (fun _ ->
  !! Accesses.shift_var ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "test_var"; cVarDef "x"];
  !! Accesses.shift_var ~factor:(trm_int 1) [nbMulti; cTopFunDef "test_var_inv"; cVarDef "s"];
  !! Accesses.shift_var ~factor:(trm_int 1) [nbMulti; cTopFunDef "test_var_inv"; cVarDef "s2"];
)
