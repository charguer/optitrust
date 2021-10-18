open Optitrust
open Target
let _ = Run.script_cpp (fun _ ->

  !! Rewrite_basic.compute_inside [ cLabelBody "block1"];
  !! Rewrite_basic.compute_inside [ cLabelBody "block2"];
  !! Rewrite_basic.compute_inside [ cLabelBody "block3"];
  
)