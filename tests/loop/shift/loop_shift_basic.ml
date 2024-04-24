open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp(fun _ ->
  !! Loop_basic.shift "i2" (ShiftBy (trm_int 2)) [cFunBody "main"; cFor "i"];
  !! Loop_basic.shift "j2" StartAtZero [cFunBody "main"; cFor "j"];
  (* FIXME:
  !! Loop_basic.shift "k2" (ShiftBy (expr "shift")) [cFunBody "main"; cFor "k"];
  !! Loop_basic.shift "l2" (ShiftBy (expr "shift")) [cFunBody "main"; cFor "l"];
  !! Loop_basic.shift "m2" (StopAt (expr "N")) [cFunBody "main"; cFor "m"]; *)
  !! Loop_basic.shift "m3" (StartAt (trm_int 4)) [cFunBody "main"; cFor "m"]; (* "m2" *)

  !! Loop_basic.shift "j" (ShiftBy (trm_int 2)) [cFunBody "excl_array"; cFor "i"];
)
