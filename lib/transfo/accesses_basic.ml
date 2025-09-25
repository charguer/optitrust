open Prelude
open Target

type transform_ret = {
  typedvar : (var * typ option) option ref;
  matched_pre : formula list ref;
  others_pre : formula list ref;
  pure_pre : resource_item list ref;
  matched_post : formula list ref;
  others_post : formula list ref;
  pure_post : resource_item list ref;
}

(** <private> *)
let transform_on (f_get : trm -> trm) (f_set : trm -> trm)
  (to_prove : trm list)
  (address_pattern : compiled_pattern)
  (mark_to_prove : mark)
  (mark_preprocess_span : mark) (mark_postprocess_span : mark)
  (mark_handled_addresses : mark)
  (ret : transform_ret)
  (span : Dir.span) (seq : trm) : trm =
  let rec identify_typedvar addr =
    Pattern.pattern_match addr [
      Pattern.(trm_array_access !__ !__) (fun base index () ->
        identify_typedvar base
      );
      Pattern.(trm_struct_access !__ !__) (fun base field () ->
        identify_typedvar base
      );
      Pattern.(trm_var !__) (fun v () ->
        match !(ret.typedvar) with
        | None ->
          ret.typedvar := Some (v, addr.typ)
        | Some (stored_v, stored_typ) ->
          if not (var_eq stored_v v) then trm_fail addr "expected all addresses to be based on same inner pointer variable";
          match (stored_typ, addr.typ) with
          | _, None -> ()
          | None, _ -> ret.typedvar := Some (v, addr.typ)
          | Some t1, Some t2 ->
            if Trm_unify.are_same_trm t1 t2
            then ()
            else trm_fail addr (sprintf "addresses are on same inner pointer variable `%s`, but types vary: %s != %s" (var_to_string v) (Ast_to_c.typ_to_string t1) (Ast_to_c.typ_to_string t2))
      );
      Pattern.__ (fun () ->
        trm_fail addr "unexpected address constructors"
      )
    ]
  in
  let address_matches addr =
    let matches = trm_matches_pattern addr address_pattern in
    if matches then identify_typedvar addr;
    matches
  in
  let rec fix_inside_span t =
    let open Resource_formula in
    Pattern.pattern_match t [
      Pattern.(trm_get !__) (fun addr () ->
        Pattern.when_ (address_matches addr);
        f_get (trm_get (trm_add_mark mark_handled_addresses addr))
      );
      Pattern.(trm_set !__ !__) (fun addr value () ->
        Pattern.when_ (address_matches addr);
        trm_set (trm_add_mark mark_handled_addresses addr)
          (f_set (trm_map fix_inside_span value))
      );
      Pattern.(formula_points_to !__ !__) (fun addr value () ->
        Pattern.when_ (address_matches addr);
        formula_points_to (trm_add_mark mark_handled_addresses addr) value
      );
      Pattern.(formula_uninit_cell !__) (fun addr () ->
        Pattern.when_ (address_matches addr);
        formula_uninit_cell (trm_add_mark mark_handled_addresses addr)
      );
      Pattern.__ (fun () ->
        trm_map fix_inside_span t
      )
    ]
  in
  let fix_one_resource_item
   (matched_ret : formula list ref) (others_ret : formula list ref)
   (f : trm -> trm) ((_, formula) : resource_item) : trm option =
    let open Resource_formula in
    let is_uninit = is_formula_uninit formula in
    let is_read_only = Option.is_some (formula_read_only_inv formula) in
    let rec aux acc_rev_ranges current_formula =
      Pattern.pattern_match current_formula [
        Pattern.(formula_group !__ (formula_range !__ !__ !__) !__) (fun index start stop step body () ->
          aux ({ index; start; stop; step; direction = DirUp } :: acc_rev_ranges) body
        );
        Pattern.(formula_any_cell !__) (fun addr () ->
          Pattern.when_ (address_matches addr);
          matched_ret := formula :: !matched_ret;
          if is_read_only then trm_fail formula "can't scale read-only resource in-place";
          if is_uninit then None else begin
            let preserves = [current_formula] in
            let fix_body = trm_set addr (f (trm_get addr)) in
            Some (Matrix_core.pointwise_fors ~preserves (List.rev acc_rev_ranges) fix_body)
          end
        );
        Pattern.__ (fun () ->
          (* let rec occur_check formula =
            if address_matches formula
            then trm_fail formula "found matching address in unsupported formula";
            trm_iter occur_check formula
          in
          occur_check formula; *)
          others_ret := formula :: !others_ret;
          None
        )
      ]
    in
    aux [] formula
  in
  let fix_outside_span
   (matched_ret : formula list ref) (others_ret : formula list ref)
   (f : trm -> trm) (res : resource_set) =
    List.filter_map (fix_one_resource_item matched_ret others_ret f) res.linear
  in
  update_span_helper span seq (fun span_instrs ->
    let (res_start, res_stop) = Resources.around_instrs span_instrs in
    ret.pure_pre := res_start.pure;
    ret.pure_post := res_stop.pure;
    let fix_start = fix_outside_span ret.matched_pre ret.others_pre f_set res_start in
    let fix_stop = fix_outside_span ret.matched_post ret.others_post f_get res_stop in
    let m0 = mark_to_prove in
    let (m1, m2) = span_marks mark_preprocess_span in
    let (m3, m4) = span_marks mark_postprocess_span in
    [ Mark m0; TrmList to_prove; Mark m1; TrmList fix_start; Mark m2;
      TrmMlist (Mlist.map fix_inside_span span_instrs);
      Mark m3; TrmList fix_stop; Mark m4 ]
  )

(** [transform f_get f_set]: expects the target [tg] to point at a sequence span.
    The transformation will search for all get and set operations on the given address pattern
    and transform them with [f_get] and [f_set], respectively.
    The transformation may also insert preprocessing and postprocessing steps.

    For correctness, [f_get] and [f_set] must be inverses and pure.
    It is checked that all of the transformed gets and sets operate on resources found at the begining of the scope. *)
let%transfo transform (f_get : trm -> trm) (f_set : trm -> trm)
  ?(to_prove : trm list = [])
  ~(address_pattern : compiled_pattern)
  ?(mark_to_prove : mark = no_mark)
  ?(mark_preprocess : mark = no_mark) ?(mark_postprocess : mark = no_mark)
  (tg : target) : unit =
  Marks.with_marks (fun next_mark -> Target.iter (fun p ->
    let (p_seq, span) = Path.extract_last_dir_span p in
    let (mark_to_prove, mark_preprocess, mark_postprocess, mark_handled_resources) =
      if !Flags.check_validity then begin
        (Mark.reuse_or_next next_mark mark_to_prove,
         Mark.reuse_or_next next_mark mark_preprocess,
         Mark.reuse_or_next next_mark mark_postprocess,
         next_mark ())
      end else
        (mark_to_prove, mark_preprocess, mark_postprocess, no_mark)
    in
    let ret = {
      typedvar = ref None;
      matched_pre = ref [];
      others_pre = ref [];
      pure_pre = ref [];
      matched_post = ref [];
      others_post = ref [];
      pure_post = ref [];
    } in
    Target.apply_at_path (transform_on f_get f_set to_prove address_pattern mark_to_prove mark_preprocess mark_postprocess mark_handled_resources ret span) p_seq;
    if !Flags.check_validity then begin
      (* TODO: factorize with local_name, should this be a Resource.assert_??? feature? may also be decomposed via elim_reuse? *)
      let error = "did not find on which inner pointer variable addresses where based" in
      let (v, ty_opt) = Option.unsome ~error !(ret.typedvar) in
      let typ = Option.unsome ~error ty_opt in
      Trace.without_resource_computation_between_steps (fun () ->
      let f_body_mark = next_mark () in
      step_backtrack ~discard_after:true (fun () ->
        let f = new_var "isolate_addr" in
        let v_tr = new_var (v.name ^ "_tr") in
        Target.apply_at_path (fun t_seq ->
          let resolve_at tg =
            let idxs = Target.resolve_target_between_children tg t_seq in
            match idxs with
            | [i] -> i
            | _ -> failwith "expected a single index"
          in
          let span: Dir.span = {
            start = resolve_at [cMarkSpanStop mark_preprocess];
            stop = resolve_at [cMarkSpanStart mark_postprocess];
          } in
          let formulas_to_res = List.map (fun r -> Resource_formula.new_anon_hyp (), r) in
          (* let linear_original res = formulas_to_res (
            (Resource_formula.formula_cell_var ~typ v_tr) :: res
          ) in *)
          let isolated_linear res = formulas_to_res (
            List.map (trm_subst_var v (trm_var ~typ v_tr)) res
          ) in
          update_span_helper span t_seq (fun instrs ->
            let isolated_pre = isolated_linear !(ret.matched_pre) in
            let isolated_post = isolated_linear !(ret.matched_post) in
            let others_pre = formulas_to_res !(ret.others_pre) in
            let others_post = formulas_to_res !(ret.others_post) in
            let pre = Resource_set.make ~pure:(List.filter (fun (h, f) -> f = Resource_formula.typ_frac) !(ret.pure_pre)) ~linear:(isolated_pre @ others_pre) () in
            (* TODO: Add ensured linear vars to post.pure *)
            let post = Resource_set.make (*~pure:(List.filter (fun (h, f) -> f <> Resource_formula.typ_frac) !(ret.pure_post))*) ~linear:(isolated_post @ others_post) () in
            let post = { post with linear = snd (Resource_computation.delete_stack_allocs (Mlist.to_list instrs) post) } in
            let contract = FunSpecContract { pre; post } in
            let f_body = trm_add_mark f_body_mark (trm_copy (trm_seq instrs)) in
            let f_def = trm_let_fun ~contract f typ_unit [(v_tr, typ)] f_body in
            (* TODO: instead of duplicating code, call f, but deactivate stack deallocation in the body of f ? *)
            (* let f_call = trm_apps (trm_var f) [trm_var v] in *)
            [Trm f_def; (* Trm f_call; *) TrmMlist instrs]
          )
        ) p_seq;
        (* DEBUG: Show.(trm ~style:(internal ~print_var_id:false ()) (get_trm_at_exn (target_of_path p_seq))); *)
        Target.iter (fun p ->
          Target.apply_at_path (trm_subst_var v (trm_var ~typ v_tr)) p
        ) [nbAny; cMark f_body_mark; cMark mark_handled_resources];
        Resources.ensure_computed_at p_seq;
      ));
      Trace.justif "all of the transformed gets and sets operate on resources found at the begining of the scope"
    end;
  ) tg)

(** <private> *)
let transform_immut_on
  (f_init : trm -> trm) (f_use : trm -> trm) (to_prove : trm list)
  (i : int) (seq : trm) : trm =
  let instrs, result = trm_inv ~error:"expected seq" trm_seq_inv seq in
  let var = ref dummy_var in
  let update let_t =
    Pattern.pattern_match let_t [
      Pattern.(trm_let !__ !__ !__) (fun v typ init () ->
        var := v;
        trm_seq_nobrace_nomarks (to_prove @ [
          trm_let ~annot:let_t.annot (v, typ) (f_init init)
        ])
      )
    ]
  in
  let rec fix t =
    let var = !var in
    Pattern.pattern_match t [
      Pattern.(trm_specific_var var) (fun () ->
        f_use t
      );
      Pattern.__ (fun () ->
        trm_map fix t
      )
    ]
  in
  let instrs' = Mlist.update_at_index_and_fix_beyond i update fix instrs in
  trm_seq ~annot:seq.annot ?result instrs'

(** like {!transform}, but for immutable variables.
    target should be variable decl. *)
let%transfo transform_immut (f_init : trm -> trm) (f_use : trm -> trm) (to_prove : trm list) (tg : target) : unit =
  Target.iter (fun p ->
    Nobrace_transfo.remove_after (fun () ->
    let (i, p_seq) = Path.index_in_seq p in
    Target.apply_at_path (transform_immut_on f_init f_use to_prove i) p_seq
    )
  ) tg

(** [scale ~inv ~factor tg]: this transformation just calls the [transform] function  with [f_get] and [f_set] args
   defined as a multiplication and a division operation respectively. If [inv] is set to true then these two
   operations will be swapped. *)
let%transfo scale ?(inv:bool=false) ~(factor:trm)
  ~(address_pattern : compiled_pattern)
  ?(mark : mark = no_mark)
  ?(mark_to_prove : mark = no_mark)
  ?(mark_preprocess : mark = no_mark) ?(mark_postprocess : mark = no_mark)
  (tg : target) : unit =
  if !Flags.check_validity then
    if not (Resources.trm_is_pure factor) then
      trm_fail factor "basic variable scaling does not support non-pure arguments";
  Trace.justif "factor is pure and will be proved != 0";
  let typ = Option.unsome ~error:"factor needs to have a known type" factor.typ in
  let op_get, op_set = if inv then (trm_mul, trm_exact_div) else (trm_exact_div, trm_mul) in
  let f_get t = trm_add_mark mark (op_get ~typ t factor) in
  let f_set t = trm_add_mark mark (op_set ~typ t factor) in
  let to_prove = [Resource_trm.to_prove Resource_formula.(formula_neq ~typ factor (trm_int ~typ 0))] in
  transform f_get f_set ~to_prove ~address_pattern ~mark_to_prove ~mark_preprocess ~mark_postprocess tg

let%transfo scale_immut ?(inv : bool = false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  if !Flags.check_validity then
    if not (Resources.trm_is_pure factor) then
      trm_fail factor "basic variable scaling does not support non-pure arguments";
  Trace.justif "factor is pure and will be proved != 0";
  let typ = Option.unsome ~error:"Arith.scale: factor needs to have a known type" factor.typ in
  let op_get, op_set = if inv then (trm_mul, trm_exact_div) else (trm_exact_div, trm_mul) in
  let f_use t = trm_add_mark mark (op_get ~typ t factor) in
  let f_init t = trm_add_mark mark (op_set ~typ t factor) in
  let to_prove = [Resource_trm.to_prove Resource_formula.(formula_neq ~typ factor (trm_int ~typ 0))] in
  transform_immut f_init f_use to_prove tg

(** [shift ~inv ~factor tg]: this transformation just calls the [transform] function with [f_get] and [f_set] args
   defined as a multiplication and a division respectively. If [inv] is set to true then these two operations
   will be swapped. *)
let%transfo shift ?(inv:bool=false) ~(factor : trm)
  ~(address_pattern : compiled_pattern)
  ?(mark : mark = no_mark)
  ?(mark_to_prove : mark = no_mark)
  ?(mark_preprocess : mark = no_mark) ?(mark_postprocess : mark = no_mark)
  (tg : target) : unit =
  if !Flags.check_validity then
    if not (Resources.trm_is_pure factor) then
      trm_fail factor "basic variable shifting does not support non-pure arguments";
  Trace.justif "factor is pure";
  let typ = Option.unsome ~error:"Arith.scale: factor needs to have a known type" factor.typ in
  let op_get, op_set = if inv then (trm_add, trm_sub) else (trm_sub, trm_add) in
  let f_get t = trm_add_mark mark (op_get ~typ t factor) in
  let f_set t = trm_add_mark mark (op_set ~typ t factor) in
  transform f_get f_set ~address_pattern ~mark_to_prove ~mark_preprocess ~mark_postprocess tg

let%transfo shift_immut ?(inv:bool=false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  if !Flags.check_validity then
    if not (Resources.trm_is_pure factor) then
      trm_fail factor "basic variable shifting does not support non-pure arguments";
  Trace.justif "factor is pure";
  let typ = Option.unsome ~error:"Arith.scale: factor needs to have a known type" factor.typ in
  let op_get, op_set = if inv then (trm_add, trm_sub) else (trm_sub, trm_add) in
  let f_use t = trm_add_mark mark (op_get ~typ t factor) in
  let f_init t = trm_add_mark mark (op_set ~typ t factor) in
  let to_prove = [] in
  transform_immut f_init f_use to_prove tg

(** [intro tg]: expects the target [tg] to be pointing at any node that could contain struct accesses, preferably
   a sequence, then it will transform all the encodings of the form struct_get (get (t), f) to
   get (struct_access (t, f)) . *)
let%transfo intro (tg : target) : unit =
  Trace.justif_always_correct ();
  Target.apply_at_target_paths (Accesses_core.intro_on) tg
