open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Accesses.shift ~factor:(trm_float 5.0) [cVarDef "x"] (* [cReadVar "x"] *)

)
