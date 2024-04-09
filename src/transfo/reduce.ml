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

(* MAYBE LATER: subrange focus
let (beg_focus, end_focus) = if !Flags.check_validity then
  let open Resource_trm in
  let open Resource_formula in
  let (_, a, b) = ghost_pair (ghost_call var_ghost_group_focus_subrange_ro [
    "sub_range", formula_range start stop (trm_int 1);
    "big_range", formula_range (trm_int 0) n (trm_int 1);
  ]) in
  (a, b)
else
  trm_seq_nobrace_nomarks [], trm_seq_nobrace_nomarks []
in *)

(** <private> *)
let focus_reduce_item (input : trm) (i : trm) (j : trm) (n : trm) (m : trm)
  (wrapped_t : trm) : trm =
  let open Resource_formula in
  if !Flags.check_validity then
    let (_, beg_focus, end_focus) = Resource_trm.(ghost_pair (ghost_call
      var_ghost_matrix2_ro_focus ["M", input; "i", i; "j", j; "m", n; "n", m])) in
    trm_seq_nobrace_nomarks [beg_focus; wrapped_t; end_focus]
  else
    wrapped_t

(** <private> *)
let elim_basic_on (mark_alloc : mark) (mark_loop : mark) (to_expr : path) (t : trm) : trm =
  let prefix = ref None in
  let updated_t = Path.apply_on_path (fun red_t ->
    let error = "expected call to reduce" in
    let (start, stop, input, n, m, j) = trm_inv ~error reduce_inv red_t in
    let acc = new_var "s" in
    let acc_typ = Option.value ~default:(typ_constr ([], "uint16_t")) red_t.typ in
    let index = new_var "i" in
    let value = (trm_cast acc_typ (Matrix_trm.get input [n; m] [trm_var index; j])) in
    let loop_range = { index; start; direction = DirUp; stop; step = Post_inc } in
    let derive_in_range = Resource_trm.(Resource_formula.(ghost (ghost_call var_ghost_subrange_to_group_in_range [
      "r1", formula_range start stop (trm_int 1);
      "r2", formula_range (trm_int 0) n (trm_int 1);
    ]))) in
    let contract = Resource_contract.(Resource_formula.(empty_loop_contract |>
      push_loop_contract_clause SharedModifies
        (new_anon_hyp (), formula_cell acc) |>
      push_loop_contract_clause SharedReads
        (new_anon_hyp (), formula_matrix input [n; m]) |>
      (* TODO: should this use require or some pure focus? *)
      push_loop_contract_clause (Exclusive Requires)
        (new_anon_hyp (), formula_in_range (trm_var index)
          (formula_range (trm_int 0) n (trm_int 1)))
    )) in
    prefix := Some [
      derive_in_range;
      trm_add_mark mark_alloc (trm_let_mut (acc, acc_typ) (trm_cast acc_typ (trm_int 0)));
      trm_add_mark mark_loop (trm_for loop_range ~contract (trm_seq_nomarks [
        (* trm_prim_compound Binop_add (trm_var acc) value *)
        focus_reduce_item input (trm_var index) j n m (
          trm_set (trm_var acc) (trm_add (trm_var_get acc) value))
      ]))
    ];
    trm_var_get acc
  ) t to_expr in
  let prefix = Option.get !prefix in
  trm_seq_nobrace_nomarks (prefix @ [updated_t])

(** [elim_basic tg]: eliminates a call to [reduce], expanding it to a for loop. *)
let%transfo elim_basic ?(mark_alloc : mark = no_mark) ?(mark_loop : mark = no_mark) (tg : target) =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let (to_instr, to_expr) = Path.path_in_instr p (Trace.ast ()) in
    Target.apply_at_path (elim_basic_on mark_alloc mark_loop to_expr) to_instr;
    if !Flags.check_validity then
      Trace.justif "valid by definition of reduce (not supporting negative ranges)"
  ) tg)

(** <private> *)
let elim_inline_on (mark_simpl : mark) (red_p : path) (t : trm) : trm =
  let focuses = ref (fun x -> x) in
  let t2 = Path.apply_on_path (fun red_t ->
    let error = "expected call to reduce" in
    let (start, stop, input, n, m, j) = trm_inv ~error reduce_inv red_t in
    let nb_elems = Arith_core.(simplify true (fun e -> gather (compute e))) (trm_sub stop start) in
    match trm_int_inv nb_elems with
    | Some nb_elems ->
      let acc_typ = Option.value ~default:(typ_constr ([], "uint16_t")) red_t.typ in
      if nb_elems = 0 then begin
        trm_cast acc_typ (trm_int 0)
      end else begin
        let values = List.init nb_elems (fun k ->
          let i = trm_add_mark mark_simpl (trm_add start (trm_int k)) in
          let focuses_prev = !focuses in
          focuses := (fun x -> focuses_prev (trm_seq_nobrace_nomarks [
            (* is_subrange(start..stop, 0..n) --> in_range(k, 0..n) *)
            Resource_formula.(Resource_trm.(ghost (ghost_call
              var_ghost_in_range_extend [
              "x", i;
              "r1", formula_range start stop (trm_int 1);
              "r2", formula_range (trm_int 0) n (trm_int 1)
            ])));
            focus_reduce_item input i j n m x
          ]));
          trm_cast acc_typ (Matrix_trm.get input [n; m] [i; j])
        ) in
        Xlist.reduce_left trm_add values
      end
    | None ->
      trm_fail red_t "expected trivially constant loop range"
  ) t red_p in
  !focuses(t2)

(** [elim_inline tg]: eliminates a call to [reduce], expanding it to an inlined expression.
    TODO: later, implement this as combi (1. unroll; 2. inline accumulator; 3. simplify zero add)
    *)
let%transfo elim_inline ?(mark_simpl : mark = no_mark) (tg : target) =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let instr_p, expr_p = Path.path_in_instr p (Trace.ast ()) in
    Target.apply_at_path (elim_inline_on mark_simpl expr_p) instr_p;
    if !Flags.check_validity then
      Trace.justif "valid by definition of reduce (not supporting negative ranges)"
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
  (* FIXME: needs refactor, do at least unrolling with combi *)
  let error = "expected for loop" in
  let (range, instrs, contract) = trm_inv ~error trm_for_inv_instrs t in
  if not (is_step_one range.step) then
    trm_fail t "non-unary loop range not yet supported";
  if range.direction <> DirUp then
    trm_fail t "non-increasing loop direction not yet supported";
  let (before_instrs, instrs) = Mlist.split i instrs in
  let (set_instr, after_instrs) = Mlist.split 1 instrs in
  let set = Mlist.nth set_instr 0 in
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
  if !Flags.check_validity = true then begin
    let usage_before = Resources.compute_usage_of_instrs before_instrs in
    let usage_red_linear = Var_map.filter (fun x u ->
      match u with
      | Required | Ensured -> false
      | _ -> true
    ) (Resources.usage_of_trm red) in
    let usage_after = Resources.compute_usage_of_instrs after_instrs in
    Resources.assert_usages_commute red.loc usage_before usage_red_linear;
    Resources.assert_usages_commute red.loc usage_red_linear usage_after;
    Trace.justif "reduction input commutes with other loop instructions";
    if List.for_all Resources.trm_is_pure [start; stop; input; n; m; j] then
      Trace.justif "all reduce arguments are pure"
    else
      trm_fail red "some reduce arguments are not pure"
  end;
  let acc = new_var "s" in
  let acc_typ = Option.get red.typ in
  let make_reduce start stop = reduce start stop input n m j in
  let step = loop_step_to_trm range.step in
  let base_reduce = make_reduce range.start (trm_add_mark mark_simpl (trm_add range.start delta)) in
  let index = trm_var range.index in
  let rec_stop = trm_add index delta in
  let rec_reduce_add_start = trm_add_mark mark_simpl (trm_sub rec_stop step) in
  let rec_reduce_add = make_reduce rec_reduce_add_start rec_stop in
  let rec_reduce_sub_start = trm_add_mark mark_simpl (trm_sub index step) in
  let rec_reduce_sub = make_reduce rec_reduce_sub_start index in
  let new_range = { range with start = trm_add_mark mark_simpl (trm_add range.start step) } in
  let open Resource_formula in
  let dispatch_ghosts ghost ro_ghost uninit_ghost formula =
    match formula_mode_inv formula with
    | RO, formula -> ro_ghost, formula
    | Uninit, formula -> uninit_ghost, formula
    | Full, formula -> ghost, formula
  in
  let add_split_ghost ghost_fn =
    (* TODO: factorize pattern with tiling ghosts *)
    List.map (fun (_, formula) ->
      let ghost, formula = ghost_fn formula in
      let i = new_var range.index.name in
      let items = formula_fun [i, typ_int ()] None (trm_subst_var range.index (trm_var i) formula) in
      Resource_trm.ghost (ghost_call ghost [
        "start", range.start; "stop", range.stop; "step", step;
        "split", new_range.start; "items", items
      ])
    )
  in
  let split_assumption = Resource_trm.(Resource_formula.[
    assume (formula_in_range new_range.start (formula_range range.start range.stop step))
  ]) in
  let subrange_assumptions = Resource_trm.(Resource_formula.[
    assume (formula_is_subrange
      (formula_range rec_reduce_add_start rec_stop step)
      (formula_range (trm_int 0) n step)
    );
    assume (formula_is_subrange
      (formula_range rec_reduce_sub_start index step)
      (formula_range (trm_int 0) n step)
    );
  ]) in
  let split_ghosts = Loop_core.(add_split_ghost (dispatch_ghosts ghost_group_split ghost_group_split_ro ghost_group_split_uninit)) contract.iter_contract.pre.linear in
  let join_ghosts = Loop_core.(add_split_ghost (dispatch_ghosts ghost_group_join ghost_group_join_ro ghost_group_join_uninit)) contract.iter_contract.post.linear in
  let pure_split_ghosts = Loop_core.(add_split_ghost (fun f -> ghost_group_split_pure, f)) contract.iter_contract.pre.pure in
  let pure_join_ghosts = Loop_core.(add_split_ghost (fun f -> ghost_group_join_pure, f)) contract.iter_contract.post.pure in
  let one_range = { range with stop = new_range.start } in
  let (unroll_ghosts, roll_ghosts) = Loop_core.unroll_ghost_pair one_range contract [range.start] in
  let new_contract = contract |>
    Resource_contract.push_loop_contract_clause SharedModifies
      (new_anon_hyp (), formula_cell acc)
  in
  let subst_start_index = trm_subst_var range.index (trm_add_mark mark_simpl range.start) in
  trm_seq_helper ~braces:false [
    TrmList split_assumption;
    TrmList pure_split_ghosts;
    TrmList split_ghosts;
    TrmList unroll_ghosts;
    TrmMlist (Mlist.map (fun t -> trm_copy (subst_start_index t)) before_instrs);
    Trm (trm_add_mark mark_alloc (trm_let_mut (acc, acc_typ) base_reduce));
    Trm (trm_set (trm_copy (subst_start_index out)) (trm_var_get acc));
    TrmMlist (Mlist.map (fun t -> trm_copy (subst_start_index t)) after_instrs);
    TrmList roll_ghosts;
    Trm (trm_for new_range ~contract:new_contract (trm_seq_helper [
      TrmList subrange_assumptions;
      TrmMlist before_instrs;
      (* trm_prim_compound Binop_add (trm_var acc) value *)
      Trm (trm_set (trm_var acc) (trm_sub (trm_add (trm_var_get acc) rec_reduce_add) rec_reduce_sub));
      Trm (trm_set out (trm_var_get acc));
      TrmMlist after_instrs;
    ]));
    TrmList pure_join_ghosts;
    TrmList join_ghosts
  ]

(** [slide_basic tg]: given a target to a call to [set(p, reduce)] within a perfectly nested loop:
    [for i in 0..n { set(p, reduce(... i ...)) }]
    allocates a variable outside the loop to compute next values based on previous values:
    [alloc s = reduce(... 0 ...); set(p[i := 0], s); for i in 1..n { set(s, f(s)); set(p, s) }]
  *)
let%transfo slide_basic ?(mark_alloc : mark = no_mark) ?(mark_simpl : mark = no_mark)
  (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let (i, loop_p) = Path.index_in_surrounding_loop p in
    Target.apply_at_path (slide_on mark_alloc mark_simpl i) loop_p;
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
