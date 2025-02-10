open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->

  !! Accesses.shift_var ~factor:(trm_int 5) [cVarDef "x"] (* [cReadVar "x"] *)

)
