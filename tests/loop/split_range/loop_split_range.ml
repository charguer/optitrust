open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp(fun _ ->

  !! Loop_basic.split_range ~nb:5 [cFunBody "f"; cFor "i"];
  !! Loop_basic.split_range ~nb:5 [cFunBody "f"; cFor "j"];

  let cut = trm_find_var "cut" [cFunBody "f"] in
  !! Loop_basic.split_range ~cut [cFunBody "f"; cFor "k"];
  !! Loop_basic.split_range ~cut [cFunBody "f"; cFor "l"];

  !! Loop_basic.split_range ~cut:(trm_int 0) [cFunBody "array_copy"; cFor "i"];

  let cut = trm_find_var "cut" [cFunBody "non_transparent_ghosts"] in
  !! Loop_basic.split_range ~cut [cFunBody "non_transparent_ghosts"; cFor "i"];
)
