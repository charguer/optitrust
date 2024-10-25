open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop.shift ~index:"i2" StartAtZero [cFor "i"];
     Loop.shift (ShiftBy (trm_find_var "shift" [])) [cFor "k"];
)
