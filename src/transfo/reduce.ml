open Prelude
open Target

(* TODO: generalize this module to any monoid/group and reduction function. *)

let reduce_var = toplevel_var "reduce_spe1"

(** [reduce]: constructs a call to [reduce]. *)
let reduce (start : trm) (stop : trm) (input : trm) (n : trm) (m : trm) (j : trm) : trm =
  trm_apps (trm_var reduce_var) [start; stop; input; n; m; j]

(** [reduce_inv]: returns the list of arguments of a call to [reduce]. *)
let reduce_inv (t : trm) : (trm * trm * trm * trm * trm * trm) option =
  match trm_apps_inv t with
  | Some (f, [start; stop; input; n; m; j]) ->
    begin match trm_var_inv f with
    | Some v when var_eq v reduce_var ->
      Some (start, stop, input, n, m, j)
    | _ -> None
    end
  | _ -> None

(** <private> *)
let elim_basic_on (mark_alloc : mark) (mark_loop : mark) (to_expr : path) (t : trm) : trm =
  let alloc_loop = ref None in
  let updated_t = Path.apply_on_path (fun red_t ->
    let error = "expected call to reduce" in
    let (start, stop, input, n, m, j) = trm_inv ~error reduce_inv red_t in
    let acc = new_var "s" in
    let acc_typ = Option.value ~default:(typ_constr ([], "uint16_t")) red_t.typ in
    let index = new_var "i" in
    let value = (trm_cast acc_typ (Matrix_trm.get input [n; m] [trm_var index; j])) in
    let loop_range = { index; start; direction = DirUp; stop; step = Post_inc } in
    alloc_loop := Some (
      trm_add_mark mark_alloc (trm_let_mut (acc, acc_typ) (trm_cast acc_typ (trm_int 0))),
      trm_add_mark mark_loop (trm_for loop_range (trm_seq_nomarks [
        (* trm_prim_compound Binop_add (trm_var acc) value *)
        trm_set (trm_var acc) (trm_add (trm_var_get acc) value)
      ])));
    trm_var_get acc
  ) t to_expr in
  let (alloc, loop) = Option.get !alloc_loop in
  (* TODO: if !Flags.check_validity then
    generate group_focus_subrange_ro scoped ghost
    is valid by definition of reduce (not supporting negative ranges)
     *)
  trm_seq_nobrace_nomarks [alloc; loop; updated_t]

(** [elim_basic tg]: eliminates a call to [reduce], expanding it to a for loop. *)
let%transfo elim_basic ?(mark_alloc : mark = no_mark) ?(mark_loop : mark = no_mark) (tg : target) =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let (to_instr, to_expr) = Path.path_in_instr p (Trace.ast ()) in
    Target.apply_at_path (elim_basic_on mark_alloc mark_loop to_expr) to_instr
  ) tg)

(** <private> *)
let elim_inline_on (mark_simpl : mark) (red_t : trm) : trm =
  let error = "expected call to reduce" in
  let (start, stop, input, n, m, j) = trm_inv ~error reduce_inv red_t in
  let nb_elems = Arith_core.(simplify true (fun e -> gather (compute e))) (trm_sub stop start) in
  (* TODO: if !Flags.check_validity then
    generate group_focus_subrange_ro scoped ghost and/or unrolled version
    is valid because unroll is valid and by definition of reduce (not supporting negative ranges)
     *)
  match trm_int_inv nb_elems with
  | Some nb_elems ->
    let acc_typ = Option.value ~default:(typ_constr ([], "uint16_t")) red_t.typ in
    if nb_elems = 0 then begin
      trm_cast acc_typ (trm_int 0)
    end else begin
      let values = List.init nb_elems (fun k ->
        let i = trm_add_mark mark_simpl (trm_add start (trm_int k)) in
        trm_cast acc_typ (Matrix_trm.get input [n; m] [i; j])
      ) in
      Xlist.reduce_left trm_add values
    end
  | None ->
    trm_fail red_t "expected trivially constant loop range"

(** [elim_inline tg]: eliminates a call to [reduce], expanding it to an inlined expression.
    TODO: later, implement this as combi (1. unroll; 2. inline accumulator; 3. simplify zero add)
    *)
let%transfo elim_inline ?(mark_simpl : mark = no_mark) (tg : target) =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    Target.apply_at_path (elim_inline_on mark_simpl) p
  ) tg)

(** [elim_basic tg]: eliminates a call to [reduce], expanding it to a for loop.

  - [unroll]: whether the reduction loop should be unrolled
  - [inline]: whether the reduction variable should be inlined (implies [unroll])
  *)
let%transfo elim ?(unroll : bool = false) ?(inline : bool = false) (tg : target) =
  Marks.with_marks (fun next_mark ->
    Target.iter (fun p ->
      if inline then begin
        let mark_simpl = next_mark () in
        elim_inline ~mark_simpl (target_of_path p);
        Arith.default_simpl [cMark mark_simpl];
      end else begin
        let mark_loop = next_mark () in
        elim_basic ~mark_loop (target_of_path p);
        if unroll then Loop.unroll [cMark mark_loop];
      end
    ) tg
  )

(** <private> *)
let slide_on (mark_alloc : mark) (mark_simpl : mark) (i : int) (t : trm) : trm =
  let error = "expected for loop" in
  let (range, instrs, _) = trm_inv ~error trm_for_inv_instrs t in
  if not (is_step_one range.step) then
    trm_fail t "non-unary loop range not yet supported";
  if range.direction <> DirUp then
    trm_fail t "non-increasing loop direction not yet supported";
  if Mlist.length instrs <> 1 || i <> 0 then
    trm_fail t "loop with more than one instruction not yet supported";
  let set = Mlist.nth instrs 0 in
  let (out, red) = trm_inv ~error trm_set_inv set in
  let error = "expected call to reduce" in
  let (start, stop, input, n, m, j) = trm_inv ~error reduce_inv red in
  let is_index_var t =
    match trm_var_inv t with
    | Some x when var_eq x range.index -> true
    | _ -> false
  in
  if not (is_index_var start) then
    trm_fail t "only supporting reduce starting at loop index";
  let delta = begin match trm_add_inv stop with
  | Some (a, b) when is_index_var a ->
    if is_index_var b || (Option.is_none (trm_var_inv b)) then
      trm_fail t "only supporting reduce stopping at loop index + constant variable";
    b
  | _ -> trm_fail t "only supporting reduce stopping at loop index + constant variable"
  end in
  (* TODO:
    if !Flags.check_validity = true then
      check that reduce args are formula_convertible
      check that range is non-empty and well-ordered (stop > start)
  *)
  let acc = new_var "s" in
  let acc_typ = Option.get red.typ in
  let make_reduce start stop = reduce start stop input n m j in
  let step = loop_step_to_trm range.step in
  let base_reduce = make_reduce range.start (trm_add_mark mark_simpl (trm_add range.start delta)) in
  let index = trm_var range.index in
  let rec_stop = trm_add index delta in
  let rec_reduce_add = make_reduce (trm_sub rec_stop step) rec_stop in
  let rec_reduce_sub = make_reduce (trm_sub index step) index in
  let new_range = { range with start = trm_add_mark mark_simpl (trm_add range.start step) } in
  trm_seq_nobrace_nomarks [
    trm_add_mark mark_alloc (trm_let_mut (acc, acc_typ) base_reduce);
    trm_set (trm_copy (trm_subst_var range.index range.start out)) (trm_var_get acc);
    trm_for new_range (trm_seq_nomarks [
      (* trm_prim_compound Binop_add (trm_var acc) value *)
      trm_set (trm_var acc) (trm_sub (trm_add (trm_var_get acc) rec_reduce_add) rec_reduce_sub);
      trm_set out (trm_var_get acc)
    ]);
  ]

(** [slide_basic tg]: given a target to a call to [set(p, reduce)] within a perfectly nested loop:
    [for i in 0..n { set(p, reduce(... i ...)) }]
    allocates a variable outside the loop to compute next values based on previous values:
    [alloc s = reduce(... 0 ...); set(p[i := 0], s); for i in 1..n { set(s, f(s)); set(p, s) }]

    TODO: generate check that n > 0, check that reduce args are formula convertible
  *)
let%transfo slide_basic ?(mark_alloc : mark = no_mark) ?(mark_simpl : mark = no_mark)
  (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let (i, loop_p) = Path.index_in_surrounding_loop p in
    Target.apply_at_path (slide_on mark_alloc mark_simpl i) loop_p
  ) tg)

(** [slide tg]: given a target to a call to [set(p, reduce)] within a perfectly nested loop:
    [for i in 0..n { set(p, reduce(... i ...)) }]
    allocates a variable outside the loop to compute next values based on previous values:
    [alloc s = reduce(... 0 ...); for i in 1..n { set(s, f(s)); set(p, s) }]

    TODO: generate check that n > 0
  *)
let%transfo slide ?(mark_alloc : mark = no_mark) ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  Marks.with_marks (fun next_mark -> Target.iter (fun p ->
    let mark_simpl = next_mark () in
    slide_basic ~mark_alloc ~mark_simpl (target_of_path p);
    simpl [cMark mark_simpl];
  ) tg)
