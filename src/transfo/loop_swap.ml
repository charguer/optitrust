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

let swap_groups = toplevel_var "swap_groups"

let ghost_swap outer_range inner_range (_, formula) =
  let open Resource_formula in
  let before = formula_group_range outer_range (formula_group_range inner_range formula) in
  let after = formula_group_range inner_range (formula_group_range outer_range formula) in
  trm_ghost_rewrite before after (trm_var swap_groups)


(** This transformation turns:

    // stars_j R(j)
    pfor j
      consumes R(j)
      produces R'(j)
      // R(j) |- S(j,0) * stars_k P(j,k) * F(j)
      for k
          invariant S(j,k)
          consumes P(j, k)
          produces P'(j, k)
          Instrs(j, k)
      // S(j,n) * stars_k P'(j,k) * F(j) |- R'(j)
    // stars_j R'(j)

  into:

    // stars_j R(j)
    // stars_j S(j,0) * stars_j stars_k P(j,k) * stars_j F(j)
    ghost rewrite stars_j stars_k P(j,k)
                = stars_k stars_j P(j,k)
    // stars_j S(j,0) * stars_k stars_j P(j,k) * stars_j F(j)
    for k
      invariant stars_j S(j,k)
      consumes stars_j P(j,k)
      produces stars_j P'(j,k)
      pfor j
        consumes S(j,k) * P(j,k)
        produces S(j,k+1) * P'(j,k)
        Instrs(j, k)
      // stars_j S(j,k+1) * stars_j P'(j,k)
    // stars_j S(j,n) * stars_k stars_j P'(j,k) * stars_j F(j)
    ghost rewrite stars_k stars_j P'(j,k)
                = stars_j stars_k P'(j,k)
    // stars_j S(j,n) * stars_j stars_k P'(j,k) * stars_j F(j)
    // stars_j R'(j)
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

      Trace.justif "Outer loop was parallelizable (swapping loops can only remove possible interleavings)";

      let loop_ghosts = inner_contract.loop_ghosts in
      let inner_inv = inner_contract.invariant in
      let inner_pre = inner_contract.iter_contract.pre in
      let inner_post = inner_contract.iter_contract.post in
      let swaps_pre = List.map (ghost_swap outer_range inner_range) inner_pre.linear in
      let swaps_post = List.map (ghost_swap inner_range outer_range) inner_post.linear in

      (* TODO: Manage swaping in pure when pure Groups are handled *)
      assert (inner_pre.pure == []);
      assert (inner_post.pure == []);

      let new_inner_pre = Resource_set.union inner_inv inner_pre in
      let new_inner_post = Resource_set.union (Resource_set.subst_loop_range_step inner_range inner_inv) inner_post in
      let new_inner_contract = { loop_ghosts; invariant = Resource_set.empty; iter_contract = { pre = new_inner_pre; post = new_inner_post } } in

      let new_outer_inv = Resource_set.group_range outer_range inner_inv in
      let new_outer_pre = Resource_set.group_range outer_range inner_pre in
      let new_outer_post = Resource_set.group_range outer_range inner_post in
      let new_outer_contract = { loop_ghosts; invariant = new_outer_inv; iter_contract = { pre = new_outer_pre; post = new_outer_post } } in

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
    Scope.check_var_ids ();
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
let%transfo swap ?(mark_outer_loop : mark option) ?(mark_inner_loop : mark option) (tg : target) : unit =
  Target.iter (fun _ outer_loop_p ->
  Marks.with_marks (fun next_m ->
    if not !Flags.check_validity then begin
      swap_basic (target_of_path outer_loop_p);
    end else begin
      let _, seq_p = Path.index_in_seq outer_loop_p in
      let outer_loop_m = Marks.add_next_mark_on next_m outer_loop_p in
      let inner_loop_m = Marks.add_next_mark next_m [nbExact 1; Constr_paths [seq_p]; cMark outer_loop_m; cStrict; cFor ""] in
      Option.iter (fun m -> Marks.add m [Constr_paths [seq_p]; cMark outer_loop_m]) mark_outer_loop;
      Option.iter (fun m -> Marks.add m [Constr_paths [seq_p]; cMark inner_loop_m]) mark_inner_loop;

      Resources.loop_parallelize_reads (target_of_path outer_loop_p);

      let inner_seq_tg = [Constr_paths [seq_p]; cMark outer_loop_m; dBody] in
      let inner_seq_p = resolve_target_exactly_one inner_seq_tg (Trace.ast ()) in

      let pairs = Ghost_pair.elim_all_pairs_at next_m inner_seq_p in

      List.iter (fun (pair_token, begin_m, end_m) ->
        Loop_basic.fission_basic [Constr_paths [seq_p]; cMark begin_m; tAfter];
        Loop_basic.fission_basic [Constr_paths [seq_p]; cMark end_m; tBefore];
        Ghost.embed_loop [Constr_paths [seq_p]; cMark begin_m];
        Ghost.embed_loop [Constr_paths [seq_p]; cMark end_m];
      ) pairs;

      Ghost_pair.reintro_pairs_at pairs seq_p;

      let inner_loop_p = resolve_target_exactly_one [cMark inner_loop_m] (Trace.ast ()) in
      let _, outer_loop_p = Path.index_in_surrounding_loop inner_loop_p in
      swap_basic (target_of_path outer_loop_p);
    end
  )
  ) tg

let f = swap
