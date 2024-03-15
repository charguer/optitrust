open Prelude
open Target

(** <private>
  [swap_on t]: swaps the order of two nested loops, the targeted loop with the immediate inner loop,
       [t] - ast of the targeted loop. *)
let swap_on_any_loop (t : trm) : trm =
  match Internal.extract_loop t with
  | Some (loop1, body1) ->
    begin match body1.desc with
    | Trm_seq tl when Mlist.length tl = 1 ->
      let loop2 = Mlist.nth tl 0 in
      begin match Internal.extract_loop loop2 with
      | Some (loop2, body2) -> loop2 (trm_seq_nomarks [loop1 body2])
      | None -> trm_fail body1 "Loop_core..swap_aux: should target a loop with nested loop^inside"
      end
    | _ -> begin match Internal.extract_loop body1 with
           | Some (loop2, body2) -> loop2 (trm_seq_nomarks [loop1 body2])
           | None -> trm_fail body1 "Loop_core..swap_aux: should target a loop with nested inner loops"
           end
    end
  | None -> trm_fail t "Loop_core.swap_aux: should target a loop"

let var_swap_groups = toplevel_var "swap_groups"
let var_ro_swap_groups = toplevel_var "ro_swap_groups"
let var_uninit_swap_groups = toplevel_var "uninit_swap_groups"

let ghost_swap (outer_range: loop_range) inner_range (_, formula) =
  let open Resource_formula in
  let ghost_var, formula = match formula_read_only_inv formula with
  | Some { formula } -> var_ro_swap_groups, formula
  | None ->
    match formula_uninit_inv formula with
    | Some formula -> var_uninit_swap_groups, formula
    | None -> var_swap_groups, formula
  in
  let outer_var = new_var outer_range.index.name in
  let inner_var = new_var inner_range.index.name in
  let formula = trm_subst (Var_map.add outer_range.index (trm_var outer_var) (Var_map.singleton inner_range.index (trm_var inner_var))) formula in
  let items = formula_fun [outer_var, typ_int (); inner_var, typ_int ()] None formula in
  Resource_trm.ghost (ghost_call ghost_var ["outer_range", formula_loop_range outer_range; "inner_range", formula_loop_range inner_range; "items", items])


(** This transformation turns:

    // stars_j R(j) * V
    pfor j
      consumes R(j)
      produces R'(j)
      par_reads V
      // R(j) ==> SR(j,0) * UR(j) * stars_k PR(j,k) * FR(j)
      // V = SV(0) * UV * stars_k PV(k) * FV
      for k
          invariant SV(k) * SR(j,k)
          par_reads UV * UR(j)
          consumes PV(k) * PR(j, k)
          produces PV(k) * P'R(j, k)
          Instrs(j, k)
      // SR(j,n) * UR(j) * stars_k P'R(j,k) * FR(j) ==> R'(j)
    // stars_j R'(j) * V

  into:

    // stars_j R(j) * V
    // stars_j SR(j,0) * stars_j UR(j) * stars_j stars_k PR(j,k) * stars_j FR(j) * SV(0) * UV * stars_k PV(k) * FV
    ghost rewrite stars_j stars_k PR(j,k)
                = stars_k stars_j PR(j,k)
    // stars_j SR(j,0) * stars_j UR(j) * stars_k stars_j PR(j,k) * stars_j FR(j) * SV(0) * UV * stars_k PV(k) * FV
    for k
      invariant SV * stars_j SR(j,k)
      par_reads UV * stars_j UR(j)
      consumes PV(k) * stars_j PR(j,k)
      produces PV(k) * stars_j P'R(j,k)
      // SV * stars_j SR(j,k) * UV * stars_j UR(j) * PV(k) * stars_j PR(j,k)
      pfor j
        consumes UR(j) * SR(j,k) * PR(j,k)
        produces UR(j) * SR(j,k+1) * P'R(j,k)
        par_reads UV * SV * PV(k)
        Instrs(j, k)
      // SV * stars_j SR(j,k+1) * UV * stars_j UR(j) * PV(k) * stars_j P'R(j,k)
    // SV * stars_j SR(j,n) * UV * stars_j UR(j) * PV(k) * stars_k stars_j P'R(j,k) * F
    ghost rewrite stars_k stars_j P'R(j,k)
                = stars_j stars_k P'R(j,k)
    // SV * stars_j SR(j,n) * UV * stars_j UR(j) * PV(k) * stars_j stars_k P'R(j,k) * F
*)
let swap_on (t: trm): trm =
  (* TODO: refactor *)
  Pattern.pattern_match t [
    Pattern.(!(trm_for !__ (
      trm_seq (mlist (!(trm_for !__ !__ (some !__)) ^:: nil)) (* ^|
      (!(trm_for !__ !__ (some !__))) *)
      ) (some !__)))
    (fun outer_loop outer_range inner_loop inner_range body inner_contract outer_contract ->
      let open Resource_contract in
      if outer_contract.invariant <> Resource_set.empty then
        if not !Flags.check_validity then raise Pattern.Next else
        failwith "Loop.swap: the outer loop has sequential invariants";

      Trace.justif "outer loop was parallelizable (swapping loops can only remove possible interleavings)";

      let loop_ghosts = inner_contract.loop_ghosts in
      let inner_inv = inner_contract.invariant in
      let inner_par_reads = inner_contract.parallel_reads in
      let inner_pre = inner_contract.iter_contract.pre in
      let inner_post = inner_contract.iter_contract.post in

      let outer_par_reads_hyps = List.fold_left (fun acc (hyp, _) -> Var_set.add hyp acc) Var_set.empty outer_contract.parallel_reads in
      let inner_invoc = Xoption.unsome inner_loop.ctx.ctx_resources_contract_invoc in
      let from_outer_par_reads_set = List.fold_left (fun acc { pre_hyp; inst_by } ->
          if Var_set.mem (Resource_computation.Formula_inst.origin_hyp inst_by) outer_par_reads_hyps then
            Var_set.add pre_hyp acc
          else
            acc
        ) Var_set.empty inner_invoc.contract_inst.used_linear in
      let is_from_outer_par_reads (hyp, _) = Var_set.mem hyp from_outer_par_reads_set in

      let par_lininv, group_lininv = List.partition is_from_outer_par_reads inner_inv.linear in
      let par_reads_twice, group_par_reads = List.partition is_from_outer_par_reads inner_par_reads in
      let par_iter, group_pre = List.partition is_from_outer_par_reads inner_pre.linear in
      let (_, group_post, _) = Resource_computation.subtract_linear_resource_set inner_post.linear par_iter in

      let swaps_pre = List.map (ghost_swap outer_range inner_range) group_pre in
      let swaps_post = List.map (ghost_swap inner_range outer_range) group_post in

      (* TODO: Manage swaping in pure when pure Groups are handled *)
      assert (inner_pre.pure == []);
      assert (inner_post.pure == []);
      assert (inner_inv.pure == []);

      let new_inner_pre = { outer_contract.iter_contract.pre with linear = group_pre @ group_par_reads @ group_lininv } in
      let new_inner_post = Resource_set.union { outer_contract.iter_contract.post with linear = group_post @ group_par_reads } (Resource_set.subst_loop_range_step inner_range (Resource_set.make ~linear:group_lininv ())) in
      let new_inner_par_reads = par_lininv @ par_reads_twice @ par_iter in
      let new_inner_contract = { loop_ghosts; invariant = Resource_set.empty; parallel_reads = new_inner_par_reads; iter_contract = { pre = new_inner_pre; post = new_inner_post } } in

      let new_outer_inv = Resource_set.union (Resource_set.group_range outer_range (Resource_set.make ~linear:group_lininv ())) (Resource_set.make ~linear:par_lininv ()) in
      let new_outer_par_reads = (Resource_set.group_range outer_range (Resource_set.make ~linear:group_par_reads ())).linear @ par_reads_twice in
      let new_outer_pre = Resource_set.union (Resource_set.group_range outer_range (Resource_set.make ~linear:group_pre ())) (Resource_set.make ~linear:par_iter ()) in
      let new_outer_post = Resource_set.union (Resource_set.group_range outer_range (Resource_set.make ~linear:group_post ())) (Resource_set.make ~linear:par_iter ()) in
      let new_outer_contract = { loop_ghosts; invariant = new_outer_inv; parallel_reads = new_outer_par_reads; iter_contract = { pre = new_outer_pre; post = new_outer_post } } in

      trm_seq_nobrace_nomarks (swaps_pre @
        [trm_for inner_range ~annot:inner_loop.annot ~contract:new_outer_contract (trm_seq_nomarks [
          trm_copy (trm_for outer_range ~annot:outer_loop.annot ~contract:new_inner_contract body)])] @
        swaps_post)
    );
    Pattern.(!__) (fun t ->
      assert (not !Flags.check_validity);
      swap_on_any_loop t)
  ]

(** [swap_basic tg]: expects the target [tg] to point at a loop that contains an
   immediately-nested loop. The transformation swaps the two loops. *)
let%transfo swap_basic (tg : target) : unit =
  Resources.required_for_check ();
  Nobrace_transfo.remove_after (fun () ->
    apply_at_target_paths swap_on tg
  );
  Resources.justif_correct "resources correct after swap"
  (* if !Flags.check_validity then begin
    Scope.infer_var_ids ();
  end *)

(** [swap tg]: expects the target [tg] to point at a loop that contains an
  immediately-nested loop. The transformation swaps the two loops.
  Also handles ghosts that may be around the nested loop, and __sequentially_reads parallelization.

  {v
  for i: <<< tg @outer_loop_m
    __sequentially_reads(H);
    GHOST_BEGIN(gp, g);
    for j: <<< @inner_loop_m
      body;
    GHOST_END(gp);

  -- Resources.loop_parallelize_reads -->

  GHOST_BEGIN(hp, ro_fork_group(..));
  pfor i: << @outer_loop_m
    GHOST_BEGIN(gp, g);
    for j: <<< @inner_loop_m
      body;
    GHOST_END(gp);
  GHOST_END(hp);

  -- Ghost_pair.elim_all_pairs_at -->

  GHOST_BEGIN(hp, ro_fork_group(..));
  pfor i: << @outer_loop_m
    g();
    for j: <<< @inner_loop_m
      body;
    g_rev();
  GHOST_END(hp);

  -- Loop_basic.fission_basic -->

  GHOST_BEGIN(hp, ro_fork_group(..));
  pfor i:
    g();
  pfor i:
    for j: <<< @inner_loop_m
      body;
  pfor i:
    g_rev();
  GHOST_END(hp);

  -- Ghost.embed_loop -->

  GHOST_BEGIN(hp, ro_fork_group(..));
  ghost ([&] {
    pfor i:
      g();
  });
  pfor i:
    for j: <<< @inner_loop_m
      body;
  ghost ([&] {
    pfor i:
      g_rev();
  });
  GHOST_END(hp);

  -- Ghost_pair.reintro_pairs_at -->

  GHOST_BEGIN(hp, ro_fork_group(..));
  GHOST_BEGIN(gp, [&] {
    pfor i:
      g();
  }, [&] {
    pfor i:
      g_rev();
  });
  pfor i:
    for j: <<< @inner_loop_m
      body;
  GHOST_END(gp);
  GHOST_END(hp);

  -- Loop.swap_basic -->

  GHOST_BEGIN(hp, ro_fork_group(..));
  GHOST_BEGIN(gp, [&] {
    pfor i:
      g();
  }, [&] {
    pfor i:
      g_rev();
  });
  for j: <<< @inner_loop_m
   pfor i:
      body;
  GHOST_END(gp);
  GHOST_END(hp);
  v}

   *)
let%transfo swap ?(mark_outer_loop : mark = no_mark) ?(mark_inner_loop : mark = no_mark) (tg : target) : unit =
  Target.iter (fun outer_loop_p ->
  Marks.with_marks (fun next_m ->
    if not !Flags.check_validity then begin
      swap_basic (target_of_path outer_loop_p);
    end else begin
      let _, seq_p = Path.index_in_seq outer_loop_p in
      let outer_loop_m = Marks.add_next_mark_on next_m outer_loop_p in
      let inner_loop_m = Marks.add_next_mark next_m [nbExact 1; Constr_paths [seq_p]; cMark outer_loop_m; cStrict; cFor ""] in
      Marks.add mark_outer_loop [Constr_paths [seq_p]; cMark outer_loop_m];
      Marks.add mark_inner_loop [Constr_paths [seq_p]; cMark inner_loop_m];

      Resources.loop_minimize (target_of_path outer_loop_p);
      Resources.detach_loop_ro_focus [Constr_paths [seq_p]; cMark inner_loop_m];

      let inner_seq_tg = [Constr_paths [seq_p]; cMark outer_loop_m; dBody] in
      let inner_seq_p = resolve_target_exactly_one inner_seq_tg in

      let pairs = Ghost_pair.elim_all_pairs_at next_m inner_seq_p in

      let loop_pairs = List.map (fun (pair_token, begin_m, end_m) ->
        Loop_basic.fission_basic [Constr_paths [seq_p]; cMark begin_m; tAfter];
        Loop_basic.fission_basic [Constr_paths [seq_p]; cMark end_m; tBefore];
        let loop_begin_m = next_m () in
        let loop_end_m = next_m () in
        Ghost.embed_loop ~mark:loop_begin_m [Constr_paths [seq_p]; cFor ~body:[cMark begin_m] ""];
        Ghost.embed_loop ~mark:loop_end_m [Constr_paths [seq_p]; cFor ~body:[cMark end_m] ""];
        (pair_token, loop_begin_m, loop_end_m)
      ) pairs in

      Ghost_pair.reintro_pairs_at loop_pairs seq_p;

      let inner_loop_p = resolve_target_exactly_one [cMark inner_loop_m] in
      let _, outer_loop_p = Path.index_in_surrounding_loop inner_loop_p in
      swap_basic (target_of_path outer_loop_p);
    end
  )
  ) tg

let f = swap
