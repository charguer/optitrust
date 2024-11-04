open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp(fun _ ->
  !! Loop.shift_range ~index:"i_s" (ShiftBy (trm_int 2)) [cFunDef "f"; cFor "i"];
  !! Loop.shift_range (ShiftBy (trm_int 2)) [cFunDef "f"; cFor "i2"];
  !! Loop.shift_range ~index:"j2" StartAtZero [cFunDef "f"; cFor "j"];
  !! Loop.shift_range ~index:"k2" ~inline:false (ShiftBy (trm_find_var "shift" [])) [cFunDef "f"; cFor "k"];
  (* FIXME:
  !! Loop.shift ~reparse:true (ShiftBy (expr "shift")) [cFor "l"]; *)
  (* FIXME:
  !! Loop.shift StartAtZero [cFor "i"]; *)

  !! Loop.shift_range (StopAt (trm_find_var "N" [cFunDef "ghost_in_range"])) [cFunDef "ghost_in_range"; cFor "m"];
  !! Loop.shift_range (StartAt (trm_int 8)) [cFunDef "ghost_in_range"; cFor "m"];

  (* FIXME: ~simpl *)
  !! Loop.shift_range ~simpl:(fun _ -> ()) (ShiftBy (trm_int 2)) [cFunDef "arrays"; cFor "i"];
)
