open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.shift "i2" StartAtZero [cFor "i"];
     Loop_basic.shift "k2" (ShiftBy (expr "shift")) [cFor "k"];
)
