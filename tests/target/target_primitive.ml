open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  show [cPrimFun (Prim_binop Binop_add)];
  show [cPrimFun (Prim_binop Binop_mul)];
  show [cPrimFun (Prim_binop Binop_div)];
  show [cPrimFun (Prim_binop Binop_mod)];
)