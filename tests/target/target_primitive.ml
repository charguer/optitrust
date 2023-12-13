open Optitrust
open Target
open Prelude

let _ = Flags.keep_marks_added_by_target_show := true

let _ = Run.script_cpp (fun _ ->

  show [cPrimFun (Prim_binop Binop_add)];
  show [cPrimFun (Prim_binop Binop_mul)];
  show [cPrimFun (Prim_binop Binop_div)];
  show [cPrimFun (Prim_binop Binop_mod)];
)
