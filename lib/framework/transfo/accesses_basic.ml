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
  (span : Dir.span) (seq : trm) : trm =
  let address_matches addr =
    Option.is_some (unify_trm addr address_pattern pattern_evars)
  in
  let rec fix_inside_span t =
    let open Resource_formula in
    Pattern.pattern_match t [
      Pattern.(trm_get !__) (fun addr () ->
        Pattern.when_ (address_matches addr);
        f_get t
      );
      Pattern.(trm_set !__ !__) (fun addr value () ->
        Pattern.when_ (address_matches addr);
        trm_set addr (f_set (trm_map fix_inside_span value))
      );
      Pattern.(formula_cell !__) (fun addr () ->
        Pattern.when_ (address_matches addr);
        t
      );
      Pattern.__ (fun () ->
        if address_matches t
        then trm_fail t "target addresses are used outside of get/set operations and model relations"
        else trm_map fix_inside_span t
      )
    ]
  in
  let fix_one_resource_item (f : trm -> trm) ((_, formula) : resource_item) : trm option =
    let open Resource_formula in
    let (mode, inner_formula) = formula_mode_inv formula in
    let rec aux acc_rev_ranges formula =
      Pattern.pattern_match formula [
        Pattern.(formula_group (formula_range !__ !__ !__) (trm_fun (pair !__ __ ^:: nil) __ !__ __)) (fun start stop step index body () ->
          aux ({ index; start; stop; step; direction = DirUp } :: acc_rev_ranges) body
        );
        Pattern.(formula_cell !__) (fun addr () ->
          Pattern.when_ (address_matches addr);
          let modifies = [formula] in
          let fix_body = trm_set addr (f (trm_get addr)) in
          Some (Matrix_core.pointwise_fors ~modifies (List.rev acc_rev_ranges) fix_body)
        );
        Pattern.__ (fun () ->
          let rec occur_check formula =
            if address_matches formula
            then trm_fail formula "found matching address in unsupported formula";
            trm_iter occur_check formula
          in
          occur_check formula;
          None
        )
      ]
    in
    match mode with
    | Uninit -> None
    (* Only if resource matches:
      | RO -> trm_fail formula "can't scale read-only resource in-place"
      currently, will generate code that does not typecheck with no generic message
    *)
    | _ (* Full *) -> aux [] inner_formula
  in
  let fix_outside_span (f : trm -> trm) (res : resource_set) =
    List.filter_map (fix_one_resource_item f) res.linear
  in
  update_span_helper span seq (fun span_instrs ->
    let (res_start, res_stop) = Resources.around_instrs span_instrs in
    let fix_start = fix_outside_span f_set res_start in
    let fix_stop = fix_outside_span f_get res_stop in
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

    For correctness, [f_get] and [f_set] must be inverses, and all operations on the matching addresses must be gets and sets exclusively.
    This allows predictably changing the values stored in the matching addresses, and restoring the values previously read from them. *)
let%transfo transform (f_get : trm -> trm) (f_set : trm -> trm)
  (* TODO: (address_pattern : pattern) ? *)
  ~(address_pattern : trm) ~(pattern_evars : eval varmap)
  ?(mark_preprocess : mark = no_mark) ?(mark_postprocess : mark = no_mark)
  (tg : target) : unit =
  Target.iter (fun p ->
    let (p_seq, span) = Path.extract_last_dir_span p in
    Target.apply_at_path (transform_on f_get f_set address_pattern pattern_evars mark_preprocess mark_postprocess span) p_seq
  ) tg

(* TODO: ~mark_preprocess ~mark_postprocess + span tg *)

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
  Trace.justif "all address occurences are covered, factor is pure and != 0";
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
  Trace.justif "all address occurences are covered, factor is pure";
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
