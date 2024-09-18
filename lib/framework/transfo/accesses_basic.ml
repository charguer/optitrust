open Prelude
open Target

(* DEPRECATED
(** [transform ~reparse f_get f_set tg]: expects the target [tg] to point at a trm inside a set or a get operation.
    Then the transformation will search for the first get or set operation surrounding the targeted trm and call
    the transform core transformation on that trm. If the first founded operation was a get operation then [f_get]
    will be applied on the node represented by target [tg]. If it was a set operation then [f_set] will be applied
    on the second argument of the targeted node. *)
let%transfo transform ?(reparse : bool = false) (f_get : trm -> trm) (f_set : trm -> trm) (tg: target) : unit =
  Target.iter (fun p ->
    let get_or_set_path = Internal.get_ascendant_path (fun t -> (is_get_operation t) || (is_set_operation t)) p (Trace.ast ()) in
    if get_or_set_path <> [] then Target.apply_at_path (Accesses_core.transform_on f_get f_set) get_or_set_path
  ) tg
*)

(** <private *)
let transform_on (f_get : trm -> trm) (f_set : trm -> trm)
  (address_pattern : trm) (pattern_evars : eval varmap)
  (mark_preprocess_span : mark) (mark_postprocess_span : mark)
  (mark_handled_addresses : mark)
  (typedvar_ret : (var * typ option) option ref)
  (res_pre_ret : formula list ref) (res_post_ret : formula list ref)
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
        match !typedvar_ret with
        | None ->
          typedvar_ret := Some (v, addr.typ)
        | Some (stored_v, stored_typ) ->
          if not (var_eq stored_v v) then trm_fail addr "expected all addresses to be based on same inner pointer variable";
          match (stored_typ, addr.typ) with
          | _, None -> ()
          | None, _ -> typedvar_ret := Some (v, addr.typ)
          | Some t1, Some t2 ->
            if are_same_trm t1 t2
            then ()
            else trm_fail addr (sprintf "addresses are on same inner pointer variable, but types vary: %s != %s" (Ast_to_c.typ_to_string t1) (Ast_to_c.typ_to_string t2))
      );
      Pattern.__ (fun () ->
        trm_fail addr "unexpected address constructors"
      )
    ]
  in
  let address_matches addr =
    let matches = Option.is_some (unify_trm addr address_pattern pattern_evars) in
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
      Pattern.(formula_cell !__) (fun addr () ->
        Pattern.when_ (address_matches addr);
        formula_cell (trm_add_mark mark_handled_addresses addr)
      );
      Pattern.__ (fun () ->
        trm_map fix_inside_span t
      )
    ]
  in
  let fix_one_resource_item (res_ret : formula list ref) (f : trm -> trm) ((_, formula) : resource_item) : trm option =
    let open Resource_formula in
    let (mode, inner_formula) = formula_mode_inv formula in
    let rec aux acc_rev_ranges formula =
      Pattern.pattern_match formula [
        Pattern.(formula_group (formula_range !__ !__ !__) (trm_fun (pair !__ __ ^:: nil) __ !__ __)) (fun start stop step index body () ->
          aux ({ index; start; stop; step; direction = DirUp } :: acc_rev_ranges) body
        );
        Pattern.(formula_cell !__) (fun addr () ->
          Pattern.when_ (address_matches addr);
          let res = List.fold_right (fun r acc ->
            formula_group_range r acc
          ) acc_rev_ranges formula in
          match mode with
          | Uninit ->
            res_ret := (formula_uninit res) :: !res_ret;
            None
          | RO -> trm_fail inner_formula "can't scale read-only resource in-place"
          | Full ->
            res_ret := res :: !res_ret;
            let modifies = [formula] in
            let fix_body = trm_set addr (f (trm_get addr)) in
            Some (Matrix_core.pointwise_fors ~modifies (List.rev acc_rev_ranges) fix_body)
        );
        Pattern.__ (fun () ->
          (* let rec occur_check formula =
            if address_matches formula
            then trm_fail formula "found matching address in unsupported formula";
            trm_iter occur_check formula
          in
          occur_check formula; *)
          None
        )
      ]
    in
    aux [] inner_formula
  in
  let fix_outside_span (res_ret : formula list ref) (f : trm -> trm) (res : resource_set) =
    List.filter_map (fix_one_resource_item res_ret f) res.linear
  in
  update_span_helper span seq (fun span_instrs ->
    let (res_start, res_stop) = Resources.around_instrs span_instrs in
    let fix_start = fix_outside_span res_pre_ret f_set res_start in
    let fix_stop = fix_outside_span res_post_ret f_get res_stop in
    let (m1, m2) = span_marks mark_preprocess_span in
    let (m3, m4) = span_marks mark_postprocess_span in
    [ Mark m1; TrmList fix_start; Mark m2;
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
  (* TODO: (address_pattern : pattern) ? *)
  ~(address_pattern : trm) ~(pattern_evars : eval varmap)
  ?(mark_preprocess : mark = no_mark) ?(mark_postprocess : mark = no_mark)
  (tg : target) : unit =
  Marks.with_marks (fun next_mark -> Target.iter (fun p ->
    let (p_seq, span) = Path.extract_last_dir_span p in
    let (mark_preprocess, mark_postprocess, mark_handled_resources) =
      if !Flags.check_validity then begin
        (Mark.reuse_or_next next_mark mark_preprocess,
         Mark.reuse_or_next next_mark mark_postprocess,
         next_mark ())
      end else
        (mark_preprocess, mark_postprocess, no_mark)
    in
    let typedvar_ret = ref None in
    let res_pre_ret = ref [] in
    let res_post_ret = ref [] in
    Target.apply_at_path (transform_on f_get f_set address_pattern pattern_evars mark_preprocess mark_postprocess mark_handled_resources typedvar_ret res_pre_ret res_post_ret span) p_seq;
    if !Flags.check_validity then begin
      (* TODO: factorize with local_name, should this be a Resource.assert_??? feature? may also be decomposed via elim_reuse? *)
      let error = "did not find on which inner pointer variable addresses where based" in
      let (v, ty_opt) = Option.unsome ~error !typedvar_ret in
      let typ = Option.unsome ~error ty_opt in
      Trace.without_resource_computation_between_steps (fun () ->
      step_backtrack ~discard_after:true (fun () ->
        let v_tr = new_var (v.name ^ "_tr") in
        Target.apply_at_path (fun t_seq ->
          let alloc = trm_let_mut_uninit (v_tr, Option.unsome ~error:"expected ptr typ" (typ_ptr_inv typ)) in
          (* NOTE: ghost below is a combination of changing the model of 'v_tr' and hiding matched resources on 'v'.
          let open_ws = ref [] in
          let close_ws = ref [] in
          List.iter (fun matched_res ->
            let (_, open_w, close_w) = Resource_trm.ghost_pair_hide matched_res in
            open_ws := open_w :: !open_ws;
            close_ws := close_w :: !close_ws;
          ) !matched_res_ret;
          *)
          let formulas_to_res = List.map (fun r -> Resource_formula.new_anon_hyp (), r) in
          let linear_original res = formulas_to_res (
            (Resource_formula.formula_cell_var ~typ v_tr) :: res
          ) in
          let linear_renamed res = formulas_to_res (
            List.map (trm_subst_var v (trm_var ~typ v_tr)) res
          ) in
          (* invariant: pre and post ret are the same resources modulo Uninit? *)
          let forget_addr = Resource_trm.ghost_admitted {
            pre = Resource_set.make ~linear:(linear_original !res_pre_ret) ();
            post = Resource_set.make ~linear:(linear_renamed !res_pre_ret) ();
          } in
          let remember_addr = Resource_trm.ghost_admitted {
            pre = Resource_set.make ~linear:(linear_renamed !res_post_ret) ();
            post = Resource_set.make ~linear:(linear_original !res_post_ret) ();
          } in
          let insert_at tg to_insert instrs =
            let idxs = Target.resolve_target_between_children tg (trm_seq instrs) in
            match idxs with
            | [i] ->
              Mlist.insert_sublist_at i to_insert instrs
            | _ -> failwith "expected a single index"
          in
          let instrs = trm_inv ~error:"expected sequence" trm_seq_inv t_seq in
          let new_instrs = instrs
            (* FIXME: natural pipe order doesn't work due to marks moving poorly *)
            |> insert_at [cMarkSpanStart mark_postprocess] [remember_addr]
            |> insert_at [cMarkSpanStop mark_preprocess] [forget_addr]
            |> insert_at [cMarkSpanStart mark_preprocess] [alloc]
          in
          trm_seq ~annot:t_seq.annot new_instrs
        ) p_seq;
        Target.iter (fun p ->
          Target.apply_at_path (trm_subst_var v (trm_var ~typ v_tr)) p
        ) [nbAny; cMark mark_handled_resources];
        recompute_resources ()
      ));
      Trace.justif "all of the transformed gets and sets operate on resources found at the begining of the scope"
    end;
  ) tg)

(** <private> *)
let transform_immut_on
  (f_init : trm -> trm) (f_use : trm -> trm)
  (i : int) (seq : trm) : trm =
  let instrs = trm_inv ~error:"expected seq" trm_seq_inv seq in
  let var = ref dummy_var in
  let update let_t =
    Pattern.pattern_match let_t [
      Pattern.(trm_let !__ !__ !__) (fun v typ init () ->
        var := v;
        trm_let ~annot:let_t.annot (v, typ) (f_init init)
      )
    ]
  in
  let rec fix t =
    let var = !var in
    Pattern.pattern_match t [
      Pattern.(trm_var (var_eq var)) (fun () ->
        f_use t
      );
      Pattern.__ (fun () ->
        trm_map fix t
      )
    ]
  in
  let instrs' = Mlist.update_at_index_and_fix_beyond i update fix instrs in
  trm_seq ~annot:seq.annot instrs'

(** like {!transform}, but for immutable variables.
    target should be variable decl. *)
let%transfo transform_immut (f_init : trm -> trm) (f_use : trm -> trm) (tg : target) : unit =
  Target.iter (fun p ->
    let (i, p_seq) = Path.index_in_seq p in
    Target.apply_at_path (transform_immut_on f_init f_use i) p_seq
  ) tg

(** [scale ~inv ~factor tg]: this transformation just calls the [transform] function  with [f_get] and [f_set] args
   defined as a multiplication and a division operation respectively. If [inv] is set to true then these two
   operations will be swapped. *)
let%transfo scale ?(inv:bool=false) ~(factor:trm)
  ~(address_pattern : trm) ~(pattern_evars : eval varmap)
  ?(mark : mark = no_mark)
  ?(mark_preprocess : mark = no_mark) ?(mark_postprocess : mark = no_mark)
  (tg : target) : unit =
  if !Flags.check_validity then
    if not (Resources.trm_is_pure factor) then
      trm_fail factor "basic variable scaling does not support non-pure arguments";
  Tools.warn "need to check that scaling factor != 0"; (* TODO: check / 0 *)
  Trace.justif "factor is pure and != 0";
  let typ = Option.unsome ~error:"factor needs to have a known type" factor.typ in
  let op_get, op_set = if inv then (trm_mul, trm_div) else (trm_div, trm_mul) in
  let f_get t = trm_add_mark mark (op_get ~typ t factor) in
  let f_set t = trm_add_mark mark (op_set ~typ t factor) in
  transform f_get f_set ~address_pattern ~pattern_evars ~mark_preprocess ~mark_postprocess tg

let%transfo scale_immut ?(inv : bool = false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  if !Flags.check_validity then
    if not (Resources.trm_is_pure factor) then
      trm_fail factor "basic variable scaling does not support non-pure arguments";
  Tools.warn "need to check that scaling factor != 0"; (* TODO: check / 0 *)
  Trace.justif "factor is pure and != 0";
  let typ = Option.unsome ~error:"Arith.scale: factor needs to have a known type" factor.typ in
  let op_get, op_set = if inv then (trm_mul, trm_div) else (trm_div, trm_mul) in
  let f_use t = trm_add_mark mark (op_get ~typ t factor) in
  let f_init t = trm_add_mark mark (op_set ~typ t factor) in
  transform_immut f_init f_use tg

(** [shift ~inv ~factor tg]: this transformation just calls the [transform] function with [f_get] and [f_set] args
   defined as a multiplication and a division respectively. If [inv] is set to true then these two operations
   will be swapped. *)
let%transfo shift ?(inv:bool=false) ~(factor : trm)
  ~(address_pattern : trm) ~(pattern_evars : eval varmap)
  ?(mark : mark = no_mark)
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
  transform f_get f_set ~address_pattern ~pattern_evars ~mark_preprocess ~mark_postprocess tg

let%transfo shift_immut ?(inv:bool=false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  if !Flags.check_validity then
    if not (Resources.trm_is_pure factor) then
      trm_fail factor "basic variable shifting does not support non-pure arguments";
  Trace.justif "factor is pure";
  let typ = Option.unsome ~error:"Arith.scale: factor needs to have a known type" factor.typ in
  let op_get, op_set = if inv then (trm_add, trm_sub) else (trm_sub, trm_add) in
  let f_use t = trm_add_mark mark (op_get ~typ t factor) in
  let f_init t = trm_add_mark mark (op_set ~typ t factor) in
  transform_immut f_init f_use tg

(** [intro tg]: expects the target [tg] to be pointing at any node that could contain struct accesses, preferably
   a sequence, then it will transform all the encodings of the form struct_get (get (t), f) to
   get (struct_access (t, f)) . *)
let%transfo intro (tg : target) : unit =
  Trace.justif_always_correct ();
  Target.apply_at_target_paths (Accesses_core.intro_on) tg
