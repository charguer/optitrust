open Optitrust
open Prelude

let _ = Flags.check_validity := false
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true


let _ = Run.script_cpp (fun _ ->

  bigstep "Tiling on i";
  !! ();
  !! Marks.add "foo" [cFor "i"];
  !! Loop_basic.tile (trm_int 4) ~bound:TileDivides [cFunDef "matrix_copy"; cFor "i"];
  !! ();
)

