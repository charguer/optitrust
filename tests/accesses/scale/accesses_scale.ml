open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Accesses.scale ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "test_var"; cVarDef "x"];
  (* TODO:
  !! Accesses.scale ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "test_array"; sExpr "t[0]"];
  !! Accesses.scale ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "main"; sExpr "t[i]"]; *)
)
