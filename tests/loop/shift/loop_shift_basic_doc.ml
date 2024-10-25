open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.shift "i2" StartAtZero [cFor "i"];
     Loop_basic.shift "k2" (ShiftBy (trm_find_var "shift" [])) [cFor "k"];

  !!! (); (* TODO: Find how to eliminate this reparse *)
)
