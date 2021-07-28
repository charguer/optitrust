open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  show [cLabel "body1";dBody];
  !! Function.elim_body ~rename:(Postfix "_1") [cLabel "body1"];
  !! Function.elim_body [cLabel "body2"];
  !! Function.elim_body ~rename:(Postfix "_2") [cLabel "body3"];
  !! Function.elim_body [cLabel "body4"];
)
