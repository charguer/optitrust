open Optitrust
open Target
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Accesses_basic.shift ~factor:(trm_double 5.0) [cRead ~addr:[cVar "x"]()]

)
