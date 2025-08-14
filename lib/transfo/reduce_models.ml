open Prelude
open Target

(* TODO: generalize this module to any monoid/group and reduction function. *)

let reduce_var = toplevel_var "reduce_int_sum"

let reduce (start : trm) (stop : trm) (f_elem : trm) : trm =
  trm_apps (trm_var reduce_var) [start; stop; f_elem]

let reduce_inv (t : trm) : (trm * trm * trm) option =
  match trm_apps_inv t with
  | Some (f, [start; stop; f_elem]) when trm_is_var ~var:reduce_var f ->
    Some((start, stop, f_elem))
  | _ -> None

let reduce_pat f_start f_stop f_f_elem k t =
  match reduce_inv t with
  | Some (start, stop, f_elem) ->
    let k = f_start k start in
    let k = f_stop k stop in
    let k = f_f_elem k f_elem in
    k
  | None -> raise Pattern.Next

let reduce_int_sum_slide_ghost_var = toplevel_var "reduce_int_sum_slide"

let reduce_int_sum_slide_ghost a b ap1 bp1 f_elem inside =
  let open Resource_trm in
  let open Resource_formula in
  let (b_geq_a, b_geq_a_ghost) = to_prove_bind (formula_geq ~typ:typ_int b a) in
  let (ap1_eq, ap1_eq_ghost) = to_prove_bind (formula_eq ~typ:typ_int ap1 (trm_add_int a (trm_int 1))) in
  let (bp1_eq, bp1_eq_ghost) = to_prove_bind (formula_eq ~typ:typ_int bp1 (trm_add_int b (trm_int 1))) in
  [ b_geq_a_ghost; ap1_eq_ghost; bp1_eq_ghost;
    (let v = new_var "v" in
    ghost (ghost_rewrite_linear
      ~by:(trm_apps (trm_var reduce_int_sum_slide_ghost_var) [
        a; b; ap1; bp1; f_elem;
        trm_var b_geq_a; trm_var ap1_eq; trm_var bp1_eq
      ])
      (trm_fun [v, typ_int] typ_hprop (inside (trm_var v)))))
  ]

let reduce_rw_a_b a b a' b' f_elem inside =
  let open Resource_trm in
  let open Resource_formula in
  let (a_eq, a_eq_ghost) = to_prove_bind (formula_eq ~typ:typ_int a a') in
  let (b_eq, b_eq_ghost) = to_prove_bind (formula_eq ~typ:typ_int b b') in
  [ a_eq_ghost;
    b_eq_ghost;
    (let v = new_var "v" in
    ghost (ghost_rewrite_linear ~by:(trm_var a_eq)
      (trm_fun [v, typ_int] typ_hprop (inside (reduce (trm_var v) b f_elem)))));
    (let v = new_var "v" in
    ghost (ghost_rewrite_linear ~by:(trm_var b_eq)
      (trm_fun [v, typ_int] typ_hprop (inside (reduce a' (trm_var v) f_elem)))))
  ]

(** <private> *)
let slide_on (available_sum: trm) (a: trm) (b: trm) (w: trm) (f_elem: trm)
  (ap1: trm) (p_of_i: trm -> trm)
  (k_compute_f_elem: (trm -> (trm -> trms) -> trms))
  (mark_alloc: mark) (mark_loop: mark)
  (mark_simpl: mark) (span: Dir.span) (t: trm) : trm =
  update_span_helper span t (fun instrs ->
    let sum = new_var "sum" in
    let typ = Option.unsome ~error:"expected pointer type" available_sum.typ in
    let var_i = new_var "i" in
    let loop_range = { index = var_i; direction = DirUp;
      start = ap1; stop = b; step = trm_int 1 } in
    let i = trm_var var_i in
    let pi = p_of_i i in
    let ai = trm_sub_int i (trm_int 1) in
    let bi = trm_sub_int (trm_add_int i w) (trm_int 1) in
    let ai' = i in
    let bi' = trm_add_int i w in
    let ai_next_inv = trm_sub_int (trm_add_int i (trm_int 1)) (trm_int 1) in
    let bi_next_inv = trm_sub_int (trm_add_int (trm_add_int i (trm_int 1)) w) (trm_int 1) in
    let pi_res r = Resource_formula.formula_points_to pi r in
    let sum_res r = Resource_formula.formula_points_to (trm_var sum) r in
    let contract = Resource_contract.(Resource_formula.(empty_loop_contract |>
      push_loop_contract_clause (Exclusive Writes) (new_anon_hyp (), pi_res (reduce ai' bi' f_elem)) |>
      push_loop_contract_clause (SharedModifies) (new_anon_hyp (), sum_res (reduce ai bi f_elem))
    )) in
    [
      Trm (trm_add_mark mark_alloc (trm_let_mut (sum, typ) available_sum));
      TrmList (reduce_rw_a_b
        a (trm_add_int a w)
        (trm_sub_int ap1 (trm_int 1)) (trm_sub_int (trm_add_int ap1 w) (trm_int 1))
        f_elem sum_res);
      Trm (trm_add_mark mark_loop (trm_for loop_range ~contract (trm_seq_helper [
        TrmList (
        k_compute_f_elem ai (fun prev_f_elem ->
        k_compute_f_elem bi (fun next_f_elem ->
        [trm_compound_assign ~typ Binop_add (trm_var sum) (trm_sub ~typ next_f_elem prev_f_elem)]
        )));
        TrmList (reduce_int_sum_slide_ghost ai bi ai_next_inv bi_next_inv f_elem sum_res);
        Trm (trm_set pi (trm_var_get sum));
        TrmList (reduce_rw_a_b ai_next_inv bi_next_inv ai' bi' f_elem pi_res);
      ])))
    ]
  )

(** [slide tg]: when targeting a span with [tg], replace the span with code having the following spec:

  {@cpp[
  __modifies("sum ~~> reduce_int_sum(a, a + w, f)");
  __consumes("for i in ap1..b -> &p(i) ~> UninitCell");
  __produces("for i in ap1..b -> &p(i) ~~> reduce_int_sum(i, i + w, f)");
  ]}

  with [ap1 = a + 1].

  In other words, a sum stencil of width [w] is computed over [f(i)] values,
  and stored in [p(i)] for [i] in [(a + 1)..b], assuming we already know the prior result computed for [a].
  to produce the required values, one can use a sliding window optimization where
  [sum(i) = sum(i - 1) - f(i - 1) + f(i - 1 + w)].

  *)
let%transfo slide_basic
  (available_sum: trm) (a: trm) (b: trm) (w: trm) (f_elem: trm)
  (ap1: trm) (p_of_i: trm -> trm)
  (k_compute_f_elem: (trm -> (trm -> trms) -> trms))
  ?(mark_alloc: mark = no_mark)  ?(mark_loop: mark = no_mark)
  ?(mark_simpl: mark = no_mark)
  (tg: target) : unit =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let p_seq, span = Path.extract_last_dir_span p in
    Target.apply_at_path (slide_on available_sum a b w f_elem ap1 p_of_i k_compute_f_elem mark_alloc mark_loop mark_simpl span) p_seq
  ) tg)

(** [slide tg]: when targeting a loop with [tg], identifies the following pattern:

  {@cpp[
  __modifies("sum ~~> reduce_int_sum(a, a + w, f)");
  __produces("for i in ap1..b -> &p(i) ~~> reduce_int_sum(i, i + w, f)");

  for i /* in ap1..b */ {
    [ ... ]
    for k /* in i..i + w */ {
      __spreserves("&s ~~> reduce_int_sum(i, k, f_elem)");
      [ ... ]
      s += compute_f_elem(s, k);
      [ ... ]
    }
    [ ... ]
  }

  ]}

  assuming [ap1 = a + 1], to call [slide_basic] with relevant arguments.
  in particular, autofocuses may be generated to provide resources for [compute_f_elem].
  *)
let%transfo slide
  ?(mark_alloc: mark = no_mark) (* ?(mark_loop: mark = no_mark) *)
  (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () ->
  Marks.with_marks (fun next_mark ->
  Target.iter (fun p ->
    let open Resource_trm in
    let open Resource_formula in
    let for_i = Target.resolve_path p in
    let res_before = Resources.before_trm for_i in
    let res_after = Resources.after_trm for_i in
    let usage = Resources.usage_of_trm for_i in
    let produced_res = List.filter (Resource_set.(linear_usage_filter usage keep_produced)) res_after.linear in

    (* 1. find available reduction pattern: sum ~~> reduce_int_sum(a, a + w, f_elem) *)
    let error = "could not find consumed reduction part of slide pattern" in
    let (sum, a, w, f_elem) = Option.unsome ~error (List.find_map (fun (_, r) ->
      Pattern.pattern_match_opt r [
        Pattern.(
          (formula_points_to !__ (reduce_pat !__ (trm_add !__ !__) !__))
        ) (fun sum a a2 w f_elem () ->
          Pattern.when_ (are_same_trm a a2);

          (* DEBUG: Show.trm_internal ~msg:"available" r; *)
          (sum, a, w, f_elem)
        );
      ]
    ) res_before.linear) in
    let available_sum = trm_get sum in

    (* 2. find produced reduction pattern: for i in ap1..b -> &p ~~> reduce_int_sum(i, i + w, f_elem) *)
    let error = "could not find produced reduction part of slide pattern" in
    let (i, ap1, b, pi) = Option.unsome ~error (List.find_map (fun (_, r) ->
      Pattern.pattern_match_opt r [
        Pattern.(formula_group !__ (formula_range !__ !__ (trm_int (eq 1)))
          (formula_points_to !__ (reduce_pat (trm_var !__) (trm_add (trm_var !__) !__) !__))
        ) (fun i ap1 b p i2 i3 w2 f_elem2 () ->
          Pattern.when_ ((var_eq i i2) && (var_eq i i3));
          Pattern.when_ ((are_same_trm w w2) && (are_same_trm f_elem f_elem2));

          (* DEBUG : Show.trm_internal ~msg:"produced" r; *)
          (i, ap1, b, p)
        );
      ]
    ) produced_res) in
    let p_of_i i' = trm_subst_var i i' pi in

    (* 3. find loops with [__spreserves("&s ~~> reduce_int_sum(i, k, f_elem)");] contract. *)
    let error = "expected for loop instrs" in
    let (i_range, for_i_instrs, _) = trm_inv ~error trm_for_inv_instrs for_i in
    let (k_range, for_k_instrs, for_k_contract) = begin
      Option.unsome ~error:"expected inner for loop instrs" (Mlist.find_map trm_for_inv_instrs for_i_instrs)
    end in
    let s = Option.unsome ~error:"could not find contract on reduction" (List.find_map (fun (_, r) ->
      Pattern.pattern_match_opt r [
        Pattern.(formula_points_to (trm_var !__)
          (reduce_pat (trm_var (var_eq i_range.index)) (trm_var (var_eq k_range.index)) !__)
        ) (fun s f_elem2 () ->
          Pattern.when_ (are_same_trm f_elem f_elem2);

          s
        );
      ]
    ) for_k_contract.invariant.linear) in

    (* 4. find [compute_f_elem] code and generate autofocuses. *)
    let compute_f_elem_k = Option.unsome ~error:"could not find code to compute elements" (Mlist.find_map (fun t ->
      Pattern.pattern_match_opt t [
        Pattern.(trm_compound_assign Binop_add (trm_var (var_eq s)) !__) (fun compute_f_elem () ->
          compute_f_elem
        )
      ]
    ) for_k_instrs) in
    let ret_compute_f_elem : (trm -> (trm -> trms) -> trms) ref = ref (fun new_k ret ->
      let compute_f_elem_new_k = trm_subst_var k_range.index new_k compute_f_elem_k in
      ret compute_f_elem_new_k) in
    let rec gen_focuses_for expr =
      match Matrix_trm.get_inv expr with
      | Some (matrix, dims, indices) ->
        let f = !ret_compute_f_elem in
        ret_compute_f_elem := (fun new_k ret ->
          let matrix = trm_subst_var k_range.index new_k matrix in
          let dims = List.map (trm_subst_var k_range.index new_k) dims in
          let indices = List.map (trm_subst_var k_range.index new_k) indices in
          let in_range_ghosts = List.map2 (fun i d ->
            Resource_trm.to_prove (formula_in_range i (formula_range (trm_int 0) d (trm_int 1)))
          ) indices dims in
          let (_, beg_focus, end_focus) = Resource_trm.(ghost_pair (
            Matrix_core.ghost_ro_matrix_focus ~matrix indices)) in
          in_range_ghosts @ [beg_focus] @ (f new_k ret) @ [end_focus])
      | _ -> trm_iter gen_focuses_for expr
    in
    gen_focuses_for compute_f_elem_k;

    (* 5. call [slide_basic] *)
    let mark_loop = next_mark () in
    slide_basic ~mark_alloc ~mark_loop available_sum a b w f_elem ap1 p_of_i (!ret_compute_f_elem) (target_of_path p);
    Resources.make_strict_loop_contracts [cMark mark_loop];
  ) tg))

(** [first_then_slide]: like [slide], but calls [unroll_first_iteration] first. *)
let%transfo first_then_slide ?(mark_alloc: mark = no_mark) (tg : target) : unit =
    Marks.with_marks (fun next_mark -> Target.iter (fun p ->
      let mark_loop = Marks.add_next_mark_on next_mark p in
      (* TODO: would need to make slide less syntax-driven to enable simpl here again *)
      Loop.unroll_first_iteration ~simpl:Arith.do_nothing ~mark_loop (target_of_path p);
      slide ~mark_alloc [cMark mark_loop];
    ) tg)
