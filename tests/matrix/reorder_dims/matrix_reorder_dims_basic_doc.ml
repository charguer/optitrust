open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.reorder_dims ~order:[1;0] [cFun "CALLOC2"];
  !! Matrix_basic.reorder_dims ~order:[1;0] [cFun "MINDEX2"];

)
