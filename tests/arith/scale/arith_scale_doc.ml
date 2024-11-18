open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Arith_basic.scale (trm_int 4) [cReadVar "x"];

)
