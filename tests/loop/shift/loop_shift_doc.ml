open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Loop.shift ~index:"i2" StartAtZero [cFor "i"];
     Loop.shift (ShiftBy (expr "shift")) [cFor "k"];
)
