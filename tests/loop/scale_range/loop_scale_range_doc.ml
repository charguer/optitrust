open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->

  !! Loop.scale_range ~index:"j" ~factor:(trm_int 4) [cFor "i"];

)
