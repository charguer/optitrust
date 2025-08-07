open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp(fun _ ->
  !! Loop_basic.shift_range "i2" (ShiftBy (trm_int 2)) [cFunBody "seq_array"; cFor "i"];
  !! Loop_basic.shift_range "j2" StartAtZero [cFunBody "seq_array"; cFor "j"];
  (* FIXME:
  !! Loop_basic.shift_range "k2" (ShiftBy (expr "shift")) [cFunBody "seq_array"; cFor "k"];
  !! Loop_basic.shift_range "l2" (ShiftBy (expr "shift")) [cFunBody "seq_array"; cFor "l"];
  !! Loop_basic.shift_range "m2" (StopAt (expr "N")) [cFunBody "seq_array"; cFor "m"]; *)
  !! Loop_basic.shift_range "m3" (StartAt (trm_int 4)) [cFunBody "seq_array"; cFor "m"]; (* "m2" *)

  !! Loop_basic.shift_range "j" (ShiftBy (trm_int 2)) [cFunBody "excl_array"; cFor "i"];

  !! Loop_basic.shift_range "i2" (ShiftBy (trm_int 2)) [cFunDef "non_transparent_ghosts"; cFor "i"];
)
