open Ast


(* [insert ty tg]: expects the target [tg] to point at any expression that can be casted, 
    then it will cast it to type [ty] *)
let insert (ty : typ) (tg : Target.target) =
  Target.reparse_after ~reparse:false (
  Target.apply_on_targets (Cast_core.insert ty)) tg

