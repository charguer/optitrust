open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Rewrite_basic.compute_inside [ cLabel "block1"];
  !! Rewrite_basic.compute_inside [ cLabel "block2"];
  !! Rewrite_basic.compute_inside [ cLabel "block3"];

)
