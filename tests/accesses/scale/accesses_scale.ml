open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->

  !! Accesses.scale_var ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "test_var"; cVarDef "x"];
  (* TODO:
  !! Accesses.scale ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "test_array"; sExpr "t[0]"];
  !! Accesses.scale ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "main"; sExpr "t[i]"]; *)
)
