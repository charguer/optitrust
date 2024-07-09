open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp(fun _ ->

  !! Loop_basic.split_range ~nb:5 [cFor "i"];
  !! Loop_basic.split_range ~nb:5 [cFor "j"];

  !! Loop_basic.split_range ~cut:(expr "cut") [cFor "k"];
  !! Loop_basic.split_range ~cut:(expr "cut") [cFor "l"];

  !!! (); (* TODO: Find how to eliminate this reparse *)
)
