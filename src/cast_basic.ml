open Ast

(* [insert ty tg] expects the target [tg] to be pointing at a get operation
    then it will cast the current type to [ty]
*)
let insert (ty : typ) (tg : Target.target) =
  Target.reparse_after ~reparse:false (
  Target.apply_on_targets (Cast_core.insert ty)) tg

