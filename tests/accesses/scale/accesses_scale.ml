open Optitrust
open Prelude

(* Arthur: we should be more consistent with ~addr: and ~base:  => perhpas addr is better?
   Begatim: we use base for array and struct accesses and we use addr for variables *)

let _ = Run.script_cpp (fun _ ->

  !! Accesses.scale ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "test_var"; cVar "x"];
  !! Accesses.scale ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "test_array"; sExpr "t[0]"];
  !! Accesses.scale ~factor:(trm_float 5.0) [nbMulti; cTopFunDef "main"; sExpr "t[i]"];

)
