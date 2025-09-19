open Optitrust
open Prelude

let _ = Flags.check_validity := false
let _ = Flags.recompute_resources_between_steps := false
let _ = Flags.disable_stringreprs := true


let _ = Run.script_cpp (fun _ ->

  bigstep "Tiling and coloring on i";
(*   !! Marks.add "foo" [cFor "i"]; *)
  !! ();
  !! Loop_basic.tile (trm_int 4) ~bound:TileDivides [cFunBody "depart"; cFor "i"];
  !! Loop_basic.color ~index:"ci" (trm_int 2) [cFunBody "depart"; cFor "bi"];

  bigstep "Insert the condition on the distance";
  !! Marks.add "foo" [cFunBody "depart"; cFor "i"; cFor "d"; cIf (); dThen];
  (* !! If_basic.insert ~cond:(expr "i > 0") [cFunBody "depart"; cFor "i"; cFor "d"; cIf (); dThen]; *)
)

