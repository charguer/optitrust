open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Accesses.shift ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "test_var"; cVar "x"];
  !! Accesses.shift ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "test_array"; sExpr "t[0]"];
  !! Accesses.shift ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "main"; sExpr "t[i]"];

)
