open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp(fun _ ->
  !! Loop.scale_range ~index:"i_s" ~factor:(trm_int 2) [cFunBody "f"; cFor "i"];
  !! Loop.scale_range ~factor:(trm_find_var "ratio" []) [cFunBody "f"; cFor "j"];
  !! Loop.scale_range ~factor:(trm_int 4) [cFunBody "ghost_in_range"; cFor "i"];
  !! Loop.scale_range ~factor:(trm_int 2) [cFunBody "array"; cFor "i"];
)
