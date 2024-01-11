open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.reorder_dims ~order:[2;1;0] [cFun "CALLOC3"];
  !! Matrix_basic.reorder_dims ~rotate_n:2 [cFun "MINDEX3"];

)
