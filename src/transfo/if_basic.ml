open Prelude

(* [insert cond tg]: expects the target [tg] to point at an instruction
    inside a sequence. Then it will create an if statement with the condition entered by the user
      and both its "then" and "else" branches will contain the same instruction.
    [cond] - denotes a string representing the code which will appear as the condition in the
    if statement, then this code is transformed and integrated inside the ast.
    [no_else_branch] - if true then the inserted if statement will not contain an else branch.
    Note:
      If [cond] is given as arbitrary string the flag [reparse] should be set to true. *)
let%transfo insert ?(cond : trm = trm_any_bool) ?(reparse : bool = false) ?(mark : mark = "") ?(no_else_branch : bool = false) (tg : target) : unit =
  Target.reparse_after ~reparse (Target.apply_on_targets (If_core.insert cond mark no_else_branch)) tg

let elim_true_on (t : trm) : trm =
  let error = "If_basic.elim_true_on: expected if" in
  let (_cond, th, _el) = trm_inv ~error trm_if_inv t in
  th

(* hypothesis: if condition evaluates to `true` *)
let%transfo elim_true (tg : target) : unit =
  apply_at_target_paths elim_true_on tg

let elim_false_on (t : trm) : trm =
  let error = "If_basic.elim_false_on: expected if" in
  let (_cond, _th, el) = trm_inv ~error trm_if_inv t in
  el

(* hypothesis: if condition evaluates to `false` *)
let%transfo elim_false (tg : target) : unit =
  apply_at_target_paths elim_false_on tg
