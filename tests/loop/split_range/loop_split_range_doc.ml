open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.split_range ~nb:2 [cFor "i"];
     Loop_basic.split_range ~cut:(trm_find_var "cut" []) [cFor "k"];
)
