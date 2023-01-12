open Ast

(* [insert_if cond tg]: expects the target [tg] to point at an instruction
    inside a sequence. Then it will create an if statement with the condition entered by the user
      and both its "then" and "else" branches will contain the same instruction.
    [cond] - denotes a string representing the code which will appear as the condition in the
    if statement, then this code is transformed and integrated inside the ast.
    [no_else_branch] - if true then the inserted if statement will not contain an else branch.
    Note:
      If [cond] is given as arbitrary string the flag [reparse] should be set to true. *)
let insert_if ?(cond : trm = trm_any_bool) ?(reparse : bool = false) ?(mark : mark = "") ?(no_else_branch : bool = false) : Target.Transfo.t =
  Target.reparse_after ~reparse (Target.apply_on_targets (Flow_core.insert_if cond mark no_else_branch))
