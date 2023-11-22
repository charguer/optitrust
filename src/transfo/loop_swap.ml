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
      | None -> fail body1.loc "Loop_core..swap_aux: should target a loop with nested loop^inside"
      end
    | _ -> begin match Internal.extract_loop body1 with
           | Some (loop2, body2) -> loop2 (trm_seq_nomarks [loop1 body2])
           | None -> fail body1.loc "Loop_core..swap_aux: should target a loop with nested inner loops"
           end
    end
  | None -> fail t.loc "Loop_core.swap_aux: should target a loop"

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
  Pattern.pattern_match t [
    Pattern.(trm_for !__ (trm_seq (mlist (trm_for !__ !__ (some !__) ^:: nil)) ^| trm_for !__ !__ (some !__)) (some !__))
    (fun outer_range inner_range body inner_contract outer_contract ->
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
      let new_inner_post = Resource_set.union (subst_invariant_step inner_range inner_inv) inner_post in
      let new_inner_contract = { loop_ghosts; invariant = Resource_set.empty; iter_contract = { pre = new_inner_pre; post = new_inner_post } } in

      let new_outer_inv = Resource_set.group_range outer_range inner_inv in
      let new_outer_pre = Resource_set.group_range outer_range inner_pre in
      let new_outer_post = Resource_set.group_range outer_range inner_post in
      let new_outer_contract = { loop_ghosts; invariant = new_outer_inv; iter_contract = { pre = new_outer_pre; post = new_outer_post } } in

      trm_seq_nobrace_nomarks (swaps_pre @
        [trm_for inner_range ~contract:new_outer_contract (trm_seq_nomarks [trm_copy (trm_for outer_range ~contract:new_inner_contract body)])] @
        swaps_post)
    );
    Pattern.(!__) swap_on_any_loop
  ]

(** [swap tg]: expects the target [tg] to point at a loop that contains an
   immediately-nested loop. The transformation swaps the two loops. *)
let%transfo swap (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
    apply_at_target_paths swap_on tg
  );
  if !Flags.check_validity then begin
    Scope.check_var_ids ();
  end

let f = swap
