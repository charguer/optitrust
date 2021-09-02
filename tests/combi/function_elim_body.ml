open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.elim_body ~renames:(Postfix "_1") [cLabel "body1"];
  !! Function.elim_body [cLabel "body2"];
  !! Function.elim_body ~renames:(Postfix "_2") [cLabel "body3"];
  !! Function.elim_body [cLabel "body4"];
)
