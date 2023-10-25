open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Rewrite_basic.compute [cPrimFun (Prim_binop Binop_add)];

)
