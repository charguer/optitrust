open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
  
  !! Function.elim_body ~vars:(AddSuffix "_1") [cLabelBody "body1"];
  !! Function.elim_body [cLabelBody "body2"];
  !! Function.elim_body ~vars:(AddSuffix "_2") [cLabelBody "body3"];
  !! Function.elim_body [cLabelBody "body4"];
)
