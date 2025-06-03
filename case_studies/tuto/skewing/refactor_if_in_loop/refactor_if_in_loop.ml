open Optitrust
open Prelude

let _ = Flags.check_validity := false

(** [trm_binop_full_inv t] extracts a binary operator and its operands if [t] is
    of the form [a OP b]; returns [Some (a, op, b)] or [None]. *)
let trm_extract_binop_inv t =
  match trm_apps_inv t with
  | Some (f, args) -> (
      match (trm_prim_inv f, args) with
      | Some (typ, Prim_binop op'), [ a; b ] -> Some (a, op', b)
      | _ -> None)
  | _ -> None

let reverse_comp_binop binop : binary_op =
  match binop with
  | Binop_gt -> Binop_lt
  | Binop_lt -> Binop_gt
  | Binop_ge -> Binop_le
  | Binop_le -> Binop_ge
  | _ -> binop

(** [reverse_comp ~lhs ~binop ~rhs t] checks if [t] equals [lhs] or [rhs], and
    returns the comparison in the form (side with [t], op, other side), flipping
    the operator if needed. Fails if [t] matches neither. *)
let reverse_comp ~(lhs : trm) ~(binop : binary_op) ~(rhs : trm) t =
  if are_same_trm lhs t then (lhs, binop, rhs)
  else if are_same_trm rhs t then (rhs, reverse_comp_binop binop, lhs)
  else
    trm_fail t
      "reverse_comp: Did not find the trm t as a trm of the comparaison"

let min_func (op1 : trm) (op2 : trm) : trm =
  trm_apps (trm_var (toplevel_var "min")) [ op1; op2 ]

let max_func (op1 : trm) (op2 : trm) : trm =
  trm_apps (trm_var (toplevel_var "max")) [ op1; op2 ]

(** [if_loop_switch_gen_on t] transforms a for-loop with an if-condition into a
    for-loop with adjusted bounds based on that condition. *)
let refactor_if_in_loop_on (t : trm) : trm =
  let error = "refactor_if_in_loop: Expected a for loop" in
  let l_range, body, contract = trm_inv ~error trm_for_inv t in
  let error =
    "refactor_if_in_loop: Expected a sequence of instructions inside the loop \
     body"
  in
  let instrs, res = trm_inv ~error trm_seq_inv body in
  if Mlist.length instrs > 1 then
    trm_fail t "refactor_if_in_loop: Expected exactly the pattern 'for-if'";
  let new_start = ref l_range.start in
  let new_stop = ref l_range.stop in
  let rec aux_and (t_and : trm) =
    let trm_apps =
      match t_and.desc with
      | Trm_if (condb, then_b, else_b) ->
          if not (annot_has_cstyle Shortcircuit_and t_and.annot) then
            trm_fail t "refactor_if_in_loop: Not a structure with if&&";
          aux_and condb;
          then_b
      | Trm_apps (f, args, _, _) -> t_and
      | _ ->
          trm_fail t
            "refactor_if_in_loop: The if condition is neither a nested if&& \
             nor a comparaison"
    in
    let error = "refactor_if_in_loop: Expected a comparaison trm" in
    let lhs, binop, rhs = trm_inv ~error trm_extract_binop_inv trm_apps in
    let index, binop, to_comp =
      reverse_comp ~lhs ~binop ~rhs (trm_var l_range.index)
    in
    let _ =
      match binop with
      | Binop_ge -> new_start := max_func to_comp !new_start
      | Binop_gt ->
          new_start := max_func (trm_add_int to_comp (trm_int 1)) !new_start
      | Binop_le ->
          new_stop := min_func (trm_add_int to_comp (trm_int 1)) !new_stop
      | Binop_lt -> new_stop := min_func to_comp !new_stop
      | Binop_eq ->
          new_start := to_comp;
          new_stop := trm_add_int to_comp (trm_int 1)
      | _ ->
          trm_fail t
            "refactor_if_in_loop: Expected comparison using one of the \
             following operators: <=, <, ==, >, >="
    in
    ()
  in

  let error = "refactor_if_in_loop: Expected exactly a for-if pattern" in
  let cond, then_, else_ = trm_inv ~error trm_if_inv (Mlist.nth instrs 0) in
  aux_and cond;
  trm_for
    {
      start = !new_start;
      stop = !new_stop;
      step = l_range.step;
      index = l_range.index;
      direction = l_range.direction;
    }
    then_ ~contract

let refactor_if_in_loop (tg : target) =
  apply_at_target_paths refactor_if_in_loop_on tg

let _ =
  Run.script_cpp (fun _ ->
      !!refactor_if_in_loop [ cFunBody "main"; cFor "i" ];
      !!refactor_if_in_loop [ cFunBody "main2"; cFor "i" ];
      !!refactor_if_in_loop [ cFunBody "main3"; cFor "i" ])
