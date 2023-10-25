open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Matrix.reorder_dims ~order:[2;1;0] () [cVarDef "p"];
  !! Matrix.reorder_dims ~rotate_n:2 () [cVarDef "q"];

)
