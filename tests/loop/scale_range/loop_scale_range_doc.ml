open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.scale_range ~index:"j" ~factor:(trm_int 4) [cFor "i"];

)
