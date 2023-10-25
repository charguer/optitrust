open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.tile (lit "3") ~index:"bi" ~bound:TileDivides [cFor "i"];
  !! Loop_basic.tile (lit "3") ~index:"bj" ~bound:TileBoundMin [cFor "j"];
  !! Loop_basic.tile (lit "3") ~index:"bk" ~bound:TileBoundAnd [cFor "k"];

)
