open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.tile (trm_int 2) ~index:"b${id}" ~bound:TileDivides [cFunDef "f"; cFor "x"];
  !! Loop_basic.tile (trm_int 2) ~bound:TileBoundMin [cFunDef "f"; cFor "y"];
  !! Loop_basic.tile (trm_int 2) ~bound:TileBoundAnd [cFunDef "f"; cFor "z"];
  !! Loop_basic.tile (trm_int 2) ~index:"b${id}" ~bound:TileDivides [cFunDef "f"; cFor "i"];
  !! Loop_basic.tile (trm_int 2) ~bound:TileBoundMin [cFunDef "f"; cFor "j"];
  !! Loop_basic.tile (trm_int 2) ~bound:TileBoundAnd [cFunDef "f"; cFor "k"];

  !! Resources.ensure_computed ();
  !! Loop_basic.tile (trm_int 4) ~bound:TileDivides [cFunDef "matrix_copy"; cFor "i"];
  !! Resources.ensure_computed ();

)
