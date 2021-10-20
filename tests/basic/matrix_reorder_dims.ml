open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.reorder_dims [2;1;0] [cFun "MCALLOC3"];
  !! Matrix_basic.reorder_dims [2;1;0] [cFun "MINDEX3"];
)
