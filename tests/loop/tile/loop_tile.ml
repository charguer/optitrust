open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.tile (lit "2") ~index:"b${id}" ~bound:TileDivides [cFor "x"];
  !! Loop_basic.tile (lit "2") ~bound:TileBoundMin [cFor "y"];
  !! Loop_basic.tile (lit "2") ~bound:TileBoundAnd [cFor "z"];
  !! Loop_basic.tile (lit "2") ~index:"b${id}" ~bound:TileDivides [cFor "i"];
  !! Loop_basic.tile (lit "2") ~bound:TileBoundMin [cFor "j"];
  !! Loop_basic.tile (lit "2") ~bound:TileBoundAnd [cFor "k"];

)
