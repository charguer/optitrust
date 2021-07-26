open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.tile "2" ~index:"bx" ~bound:TileBoundDivides [cFor "x"];
  !! Loop_basic.tile "2" ~bound:TileBoundMin [cFor "y"];
  !! Loop_basic.tile "2" ~bound:TileBoundAnd [cFor "z"];

  !! Loop_basic.tile "2" ~index:"ba" ~bound:TileBoundDivides [cFor "i"];
  !! Loop_basic.tile "2" ~bound:TileBoundMin [cFor "j"];
  !! Loop_basic.tile "2" ~bound:TileBoundAnd [cFor "k"];
)
