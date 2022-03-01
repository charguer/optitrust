open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->


  !! Function.inline [cFun "vect_mul"];
  !! Function.inline [cFun "vect_add"];
)

