open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.reorder_dims ~order:[1;0] [cCall "CALLOC2"];
  !! Matrix_basic.reorder_dims ~order:[1;0] [cCall "MINDEX2"];

)
