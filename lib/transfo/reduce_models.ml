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
let slide_on (available_sum : trm) (a : trm) (b : trm) (w : trm) (p_of_i : trm -> trm)
  (f_elem : trm) (k_compute_f_elem : (trm -> (trm -> trms) -> trms))
  (mark_alloc : mark) (mark_simpl : mark) (span : Dir.span) (t : trm) : trm =
  update_span_helper span t (fun instrs ->
    let sum = new_var "sum" in
    let typ = Option.unsome ~error:"expected pointer type" available_sum.typ in
    let var_i = new_var "i" in
    let loop_range = { index = var_i; direction = DirUp;
      start = trm_add_int a (trm_int 1); stop = b; step = trm_int 1 } in
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
      Trm (trm_let_mut (sum, typ) available_sum);
      Trm (trm_for loop_range ~contract (trm_seq_helper [
        TrmList (
        k_compute_f_elem ai (fun prev_f_elem ->
        k_compute_f_elem bi_next_inv (fun next_f_elem ->
        [trm_compound_assign ~typ Binop_add (trm_var sum) (trm_sub ~typ next_f_elem prev_f_elem)]
        )));
        TrmList (reduce_int_sum_slide_ghost ai bi ai_next_inv bi_next_inv f_elem sum_res);
        Trm (trm_set pi (trm_var_get sum));
        TrmList (reduce_rw_a_b ai_next_inv bi_next_inv ai' bi' f_elem pi_res);
      ]))
    ]
  )

(** [slide tg]: when targeting a span with [tg], replace the span with code having the following spec:

  {@cpp[
  __modifies("sum ~~> reduce_int_sum(a, a + w, f)");
  __consumes("for i in (a + 1)..b -> &p(i) ~> UninitCell");
  __produces("for i in (a + 1)..b -> &p(i) ~~> reduce_int_sum(i, i + w, f)");
  ]}

  In other words, a sum stencil of width [w] is computed over [f(i)] values,
  and stored in [p(i)] for [i] in [(a + 1)..b], assuming we already know the prior result computed for [a].
  to produce the required values, one can use a sliding window optimization where
  [sum(i) = sum(i - 1) - f(i - 1) + f(i - 1 + w)].

  *)
let%transfo slide_basic
  (available_sum : trm) (a : trm) (b : trm) (w : trm) (p_of_i : trm -> trm)
  (f_elem : trm) (k_compute_f_elem : (trm -> (trm -> trms) -> trms))
  ?(mark_alloc : mark = no_mark) ?(mark_simpl : mark = no_mark)
  (tg : target) : unit =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let p_seq, span = Path.extract_last_dir_span p in
    Target.apply_at_path (slide_on available_sum a b w p_of_i f_elem k_compute_f_elem mark_alloc mark_simpl span) p_seq
  ) tg)

(** [slide tg]: when targeting a span with [tg], identifies the following pattern:

  {@cpp[
  // __consumes("sum ~~> reduce_int_sum(a, a + w, f)");
  __consumes("sum ~~> reduce_int_sum(a + 1 - 1, a + 1 + w - 1, f)");
  __produces("for i in (a + 1)..b -> &p(i) ~~> reduce_int_sum(i, i + w, f)");
  ]}

  to call [slide_basic] with relevant arguments.
  *)
let%transfo slide (k_compute_f_elem : (trm -> (trm -> trms) -> trms)) (tg : target) : unit =
  Target.iter (fun p ->
    let p_seq, span = Path.extract_last_dir_span p in
    ignore (update_span_helper span (Target.resolve_path p_seq) (fun instrs ->
      let open Resource_trm in
      let open Resource_formula in
      let (res_before, res_after) = Resources.around_instrs instrs in

      (* 1. find consumed reduction pattern: sum ~~> reduce_int_sum(a + 1 - 1, a + 1 + w - 1, f_elem) *)
      let error = "could not find consumed reduction part of slide pattern" in
      let (sum, a, w, f_elem) = Option.unsome ~error (List.find_map (fun (_, r) ->
        Pattern.pattern_match_opt r [
          Pattern.(
            (formula_points_to !__ (reduce_pat
              (trm_sub (trm_add !__ (trm_int (eq 1))) (trm_int (eq 1)))
              (trm_sub (trm_add (trm_add !__ (trm_int (eq 1))) !__) (trm_int (eq 1)))
              !__))
          ) (fun sum a a2 w f_elem () ->
            Pattern.when_ (are_same_trm a a2);

            (* DEBUG: Show.trm_internal ~msg:"consumed" r; *)
            (sum, a, w, f_elem)
          );
        ]
      ) res_before.linear) in

      (* 2. find produced reduction pattern: for i in (a + 1)..b -> &p ~~> reduce_int_sum(i, i + w, f_elem) *)
      let error = "could not find produced reduction part of slide pattern" in
      let (i, b, pi) = Option.unsome ~error (List.find_map (fun (_, r) ->
        Pattern.pattern_match_opt r [
          Pattern.(formula_group !__ (formula_range (trm_add !__ (trm_int (eq 1))) !__ (trm_int (eq 1)))
            (formula_points_to !__ (reduce_pat (trm_var !__) (trm_add (trm_var !__) !__) !__))
          ) (fun i a2 b p i2 i3 w2 f_elem2 () ->
            Pattern.when_ ((var_eq i i2) && (var_eq i i3));
            Pattern.when_ ((are_same_trm a a2) && (are_same_trm w w2) && (are_same_trm f_elem f_elem2));

            (* DEBUG : Show.trm_internal ~msg:"produced" r; *)
            (i, b, p)
          );
        ]
      ) res_after.linear) in

      (* 3. call [slide_basic] *)
      let available_sum = trm_get sum in
      let p_of_i i' = trm_subst_var i i' pi in
      slide_basic available_sum a b w p_of_i f_elem k_compute_f_elem (target_of_path p);

      []
    ))
  ) tg
