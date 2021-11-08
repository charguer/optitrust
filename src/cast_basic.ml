open Ast

(* [insert ty tg] expects the target [ŧg] to point to a get operation
    then it will cast the current type to [ty]
*)
let insert ?(typ : string option) ?(typ_ast : typ option) (tg : Target.target) =
  try let ty = combine_styp typ typ_ast in
  let reparse = not (is_typ ty) in
  Target.reparse_after ~reparse (
  Target.apply_on_targets (Cast_core.insert ty)) tg
  with | Ast_and_code_provided -> fail None "insert: please choose between a string representation of type or ast of type"
| No_ast_or_code_provided -> fail None "insert: the type shoudl be entered as a string or as ast"
