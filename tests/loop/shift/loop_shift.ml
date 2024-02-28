open Optitrust
open Prelude

let _ = Run.script_cpp(fun _ ->
  !! Loop.shift ~index:"i_s" (ShiftBy (trm_int 2)) [occFirst; cFor "i"];
  !! Loop.shift (ShiftBy (trm_int 2)) [cFor "i2"];
  !! Loop.shift ~index:"j2" StartAtZero [cFor "j"];
  !! Loop.shift ~reparse:true ~index:"k2" ~inline:false (ShiftBy (expr "shift")) [cFor "k"];
  !! Loop.shift ~reparse:true (ShiftBy (expr "shift")) [cFor "l"];
  !! Loop.shift ~reparse:true (StopAt (expr "N")) [cFor "m"];
  !! Loop.shift (StartAt (trm_int 8)) [cFor "m"];
  !! Loop.shift StartAtZero [cFor "i"];
)
