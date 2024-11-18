open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Arith_basic.shift (trm_int 4) [cReadVar "x"];

)
