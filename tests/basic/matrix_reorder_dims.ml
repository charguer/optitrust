open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.reorder_dims ~order:[2;1;0] () [cFun "MCALLOC3"];
  !! Matrix_basic.reorder_dims ~rotate_n:2 () [cFun "MINDEX3"];

  (* TODO: make sure to have a combi version for reorder_dims that takes a variable definition as target,
     and figures out all reorder to perform on the allocation operation, and all accesses (read and write) *)
)
