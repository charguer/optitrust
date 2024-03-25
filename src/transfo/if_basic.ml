open Prelude

(* [insert_aux single_branch index cond t]: takes one or two instructions and create an if statement or an if else
     statment when [single_brnach] is true,
      [cond] - condition of the if statement given as string as a trm,
      [t] - ast of the outer sequence containing the instruction. *)
let insert_on (cond : trm) (mark : mark) (mark_then : mark) (mark_else : mark) (else_branch : bool) (t : trm) : trm =
  if !Flags.check_validity then begin
    if else_branch = false then trm_fail t "inserting if without else requires further checks";
    if Resources.trm_is_pure cond then
      Trace.justif "inserted condition is pure, scopes are checked by var ids"
  end;
  let t = trm_seq_enforce t in
  let else_br = if else_branch then (trm_add_mark mark_else (trm_copy t)) else trm_unit() in
  let then_br = trm_add_mark mark_then (trm_filter_mark (fun _ -> false) t) in
  trm_add_mark mark (trm_if cond then_br else_br)

(** [insert cond tg]: expects the target [tg] to point at an instruction
    inside a sequence. Then it will create an if statement with the condition entered by the user
    and both its "then" and "else" branches will contain the same instruction.
    [cond] - the code which will appear as the condition in the if statement.
    [no_else_branch] - if true then the inserted if statement will not contain an else branch.
    Note:
      If [cond] is given as arbitrary string the flag [reparse] should be set to true. *)
let%transfo insert ?(cond : trm = trm_any_bool) ?(reparse : bool = false) ?(mark : mark = no_mark) ?(mark_then : mark = no_mark) ?(mark_else : mark = no_mark) ?(else_branch : bool = true) (tg : target) : unit =
  Target.reparse_after ~reparse (Target.apply_at_target_paths (insert_on cond mark mark_then mark_else else_branch)) tg

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
