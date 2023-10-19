open Optitrust
open Target
open Prelude

let _ = Run.script_cpp(fun _ ->
  !! Loop_basic.shift "i2" (ShiftBy (trm_int 2)) [cFor "i"];
  !! Loop_basic.shift "j2" StartAtZero [cFor "j"];
  !! Loop_basic.shift "k2" (ShiftBy (expr "shift")) [cFor "k"];
  !! Loop_basic.shift "l2" (ShiftBy (expr "shift")) [cFor "l"];
  !! Loop_basic.shift "m2" (StopAt (expr "N")) [cFor "m"];
  !! Loop_basic.shift "m3" (StartAt (trm_int 4)) [cFor "m2"];
)
