open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Accesses_basic.scale ~factor:(trm_float 5.0) [cVarDef "x"] (* [cReadOrWrite ~addr:[cVar "x"] ()] *)

)
