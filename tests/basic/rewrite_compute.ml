open Optitrust
open Target
let _ = Run.script_cpp (fun _ -> 

  !! Rewrite_basic.compute [nbMulti;cPrimFun (Prim_binop Binop_add)];
  !! Rewrite_basic.compute [nbMulti;cPrimFun (Prim_binop Binop_mul)];
  !! Rewrite_basic.compute [nbMulti;cPrimFun (Prim_binop Binop_div)];
  !! Rewrite_basic.compute [nbMulti;cPrimFun (Prim_binop Binop_mod)];
)