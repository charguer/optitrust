open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp(fun _ ->
  !! Loop.shift ~index:"i_s" (ShiftBy (trm_int 2)) [occFirst; cFor "i"];
  !! Loop.shift (ShiftBy (trm_int 2)) [cFor "i2"];
  !! Loop.shift ~index:"j2" StartAtZero [cFor "j"];
  !! Loop.shift ~index:"k2" ~inline:false (ShiftBy (trm_find_var "shift" [])) [cFor "k"];
  (* FIXME:
  !! Loop.shift ~reparse:true (ShiftBy (expr "shift")) [cFor "l"]; *)
  !! Loop.shift (StopAt (trm_find_var "N" [])) [cFor "m"];
  !! Loop.shift (StartAt (trm_int 8)) [cFor "m"];
  (* FIXME:
  !! Loop.shift StartAtZero [cFor "i"]; *)
)
