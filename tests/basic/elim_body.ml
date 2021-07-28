open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function_basic.elim_body (fun x -> x ^"_1") [cLabel "body1"];
  !! Function_basic.elim_body (fun x -> x ^"_1") [cLabel "body2"];
  !! Function_basic.elim_body (fun x -> x ^"_1") [cLabel "body3"];
  !! Function_basic.elim_body (fun x -> x ^"_1") [cLabel "body4"];
)
