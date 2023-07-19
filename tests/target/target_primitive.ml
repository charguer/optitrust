open Optitrust
open Target
open Syntax

let _ = Flags.execute_show_even_in_batch_mode := true

let _ = Run.script_cpp (fun _ ->

  show [cPrimFun (Prim_binop Binop_add)];
  show [cPrimFun (Prim_binop Binop_mul)];
  show [cPrimFun (Prim_binop Binop_div)];
  show [cPrimFun (Prim_binop Binop_mod)];
)