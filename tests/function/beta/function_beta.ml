open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  (* Case when f is a normal function call *)
  !! Function.beta [cFun "f"];

  (* Case when f is an inline function call *)
  !! Variable_basic.inline ~accept_functions:true [cFunDef "g"];
  !! Function.beta ~indepth:true [];

)
