open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Accesses_basic.scale ~factor:(trm_float 5.0) [cReadOrWrite ~addr:[cVar "x"] ()]

)
