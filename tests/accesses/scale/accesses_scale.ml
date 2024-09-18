open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->

  !! Accesses.scale_var ~factor:(trm_float 5.0) [cTopFunDef "test_var"; cVarDef "x"];

  let t = trm_find_var "t" [cTopFunDef "test_array"] in
  let address_pattern = trm_array_access t (trm_int 0) in
  let pattern_evars = Var_map.empty in
  !! Accesses.scale ~factor:(trm_float 5.0) ~address_pattern ~pattern_evars [tSpanSeq [cFunBody "test_array"]];

  (* TODO:
  !! Accesses.scale ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "main"; sExpr "t[i]"]; *)
)
