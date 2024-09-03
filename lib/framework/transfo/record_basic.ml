open Prelude
open Target
include Record_core
include Record_core.Rename

let split_fields_on (typvar : typvar) (field_list : (field * typ) list)
  (span : Dir.span) (t_seq : trm) : trm =
  let instrs = trm_inv ~error:"expected seq" trm_seq_inv t_seq in
  if span.start >= span.stop then begin
    t_seq
  end else begin
    let (span_instrs, instrs_after) = Mlist.split span.stop instrs in
    let (instrs_before, span_instrs) = Mlist.split span.start span_instrs in
    (* DEBUG Show.trms ~msg:"span_instrs" (Mlist.to_list span_instrs); *)
    let ptr_typ_matches typ =
      Pattern.pattern_match typ [
        Pattern.(typ_ptr (trm_var (var_eq typvar))) (fun () -> true);
        Pattern.__ (fun () -> false)
      ]
    in
    let trm_ptr_typ_matches base =
      match base.typ with
      | None ->
        (* FIXME: this is a temporary workaround, look for the type in rest of code. *)
        let evars = ref Var_map.empty in
        let rec to_base_pattern t =
          Pattern.pattern_match t [
            Pattern.(trm_binop Binop_array_access !__ __) (fun base () ->
              let evar = new_var "evar" in
              evars := Var_map.add evar None !evars;
              trm_array_access base (trm_var evar)
            );
            Pattern.__ (fun () -> trm_map to_base_pattern t)
          ]
        in
        let base_pattern = to_base_pattern base in
        let matches_base t =
          Option.is_some (unify_trm t base_pattern !evars)
        in
        let exception TypeFound of typ in
        begin try
          Mlist.iter (fun t ->
            let rec search t =
              match base.typ with
              | Some typ when matches_base t -> raise (TypeFound typ)
              | _ -> trm_map search t
            in
            ignore (search t)
          ) span_instrs;
          Show.trm_internal base;
          failwith "could not find type of base '%s'" (Ast_to_c.ast_to_string base_pattern)
        with
        | TypeFound typ -> ptr_typ_matches typ
        end
      | Some typ -> ptr_typ_matches typ
    in
    let process_matching_resource_item process_one_cell default (h, formula) =
      let open Resource_formula in
      let (mode, formula) = formula_mode_inv formula in
      Printf.printf "R: %s\n" (Resource_computation.formula_to_string formula);
      let rec aux wrap_cell formula =
        Pattern.pattern_match formula [
          Pattern.(formula_group !__ (trm_fun (pair !__ __ ^:: nil) !__ !__ __)) (fun range idx _frettyp inner_formula () ->
            aux (fun c -> wrap_cell (trm_apps ~annot:formula.annot trm_group [range; trm_copy (formula_fun [idx, typ_int] None c)])) inner_formula
          );
          Pattern.(formula_model !__ (trm_var (var_eq var_cell))) (fun loc () ->
            Pattern.when_ (trm_ptr_typ_matches loc);
            Printf.printf "MATCH!\n";
            [process_one_cell wrap_cell mode loc]
          );
          Pattern.__ (fun () -> default formula)
        ]
      in
      aux (fun c -> c) formula
    in
    let (unfolds, folds) = if !Flags.check_validity then begin
      let open Resource_formula in
      let open Resource_contract in
      let first = Option.unsome ~error:"expected first instruction" (Mlist.nth_opt instrs 0) in
      let last = Option.unsome ~error:"expected last instruction" (Mlist.lst instrs) in
      let res_start = Resources.before_trm first in
      let res_stop = Resources.after_trm last in
      let make_admitted pure linear1 linear2 =
        Resource_trm.ghost_admitted {
          pre = Resource_set.make ~pure ~linear:linear1 ();
          post = Resource_set.make ~linear:linear2 () }
      in
      let fold_or_unfold ~(fold : bool) pure folded_linear unfolded_linear =
        if fold
        then make_admitted pure unfolded_linear folded_linear
        else make_admitted pure folded_linear unfolded_linear
      in
      let process_one_cell ~(fold : bool) wrap_cell (mode, loc) =
        let model loc = wrap_cell (formula_model loc trm_cell) in
        match mode with
        | RO ->
          (* TODO: think more about fractions
          let per_field_admits = List.map (fun (sf, ty) ->
            let frac_var, frac_ghost = new_frac () in
            let folded_res =
              formula_read_only ~frac:(trm_var frac_var) (formula_model loc trm_cell)
            in
            let unfolded_res =
              formula_read_only ~frac:(trm_var frac_var)
                (formula_model (trm_struct_access ~typ:(typ_ptr ty) loc sf) trm_cell)
            in
            let wand = formula_wand unfolded_res folded_res in
            let folded_linear = [(new_anon_hyp (), folded_res)] in
            let unfolded_linear = [
              (new_anon_hyp (), wand);
              (new_anon_hyp (), unfolded_res)] in
            if fold
            then make_admitted [frac_ghost] unfolded_linear folded_linear
            else make_admitted [frac_ghost] folded_linear unfolded_linear
          ) field_list in
          per_field_admits
            *)
          let frac_var, frac_ghost = new_frac () in
          let folded_linear = [
            (new_anon_hyp (), formula_read_only ~frac:(trm_var frac_var) (model loc))
          ] in
          let unfolded_linear = List.map (fun (sf, ty) ->
            (new_anon_hyp (), formula_read_only ~frac:(trm_var frac_var)
              (model (trm_struct_access ~typ:(typ_ptr ty) loc sf)))
          ) field_list in
          fold_or_unfold ~fold [frac_ghost] folded_linear unfolded_linear
        | Uninit ->
          let folded_linear = [(
            new_anon_hyp (), formula_uninit (model loc)
          )] in
          let unfolded_linear = List.map (fun (sf, ty) ->
            (new_anon_hyp (), formula_uninit (model
              (trm_struct_access ~typ:(typ_ptr ty) loc sf)
            ))
          ) field_list in
          fold_or_unfold ~fold [] folded_linear unfolded_linear
        | Full ->
          let folded_linear = [(
            new_anon_hyp (), model loc
          )] in
          let unfolded_linear = List.map (fun (sf, ty) ->
            (new_anon_hyp (), model
              (trm_struct_access ~typ:(typ_ptr ty) loc sf)
            )
          ) field_list in
          fold_or_unfold ~fold [] folded_linear unfolded_linear
      in
      let process_one_item ~(fold : bool) =
        process_matching_resource_item (fun wrap mode c -> process_one_cell ~fold wrap (mode, c)) (fun _ -> [])
      in
      let unfolds = List.concat_map (process_one_item ~fold:false) res_start.linear in
      let folds = List.concat_map (process_one_item ~fold:true) res_stop.linear in
      (unfolds, folds)
    end else begin
      ([], [])
    end in
    let _update_term t =
      (* TODO: factorize logic with to_variables / set_explicit ? *)
      let aux_resource_items items =
        let open Resource_formula in
        List.concat (List.concat_map (fun (h, formula) ->
          process_matching_resource_item (fun wrap _mode loc ->
            List.map (fun (sf, ty) ->
              (new_anon_hyp (), formula_map_under_mode (fun _ ->
                wrap (formula_model (trm_struct_access ~typ:(typ_ptr ty) loc sf) trm_cell)
              ) formula)
            ) field_list
          ) (fun f -> [[(h, f)]]) (h, formula)
        ) items)
      in
      let aux_resource_set res =
        { res with
          pure = aux_resource_items res.pure;
          linear = aux_resource_items res.linear }
      in
      let aux_fun_contract contract =
        { pre = aux_resource_set contract.pre;
          post = aux_resource_set contract.post }
      in
      let rec aux (t : trm) : trm =
        Pattern.pattern_match t [
          Pattern.(trm_set !__ !__) (fun base value () ->
            Pattern.when_ (trm_ptr_typ_matches base);
            let set_one (sf, ty) =
              trm_set (trm_struct_access ~typ:(typ_ptr ty) base sf)
                (trm_get (trm_struct_access ~typ:(typ_ptr ty) value sf))
            in
            trm_seq_nobrace_nomarks (List.map set_one field_list)
          );
          Pattern.(trm_struct_get !__ (trm_get !__)) (fun field base () ->
            trm_get (trm_struct_access base field)
          );
          (* TODO: also do other contracts *)
          Pattern.(trm_for !__ !__ !__) (fun range body spec () ->
            let contract = { spec with
              invariant = aux_resource_set spec.invariant;
              parallel_reads = aux_resource_items spec.parallel_reads;
              iter_contract = aux_fun_contract spec.iter_contract;
            } in
            trm_map aux (trm_for ~annot:t.annot ~contract range body)
          );
          Pattern.(trm_fun_with_contract !__ !__ !__) (fun args body contract () ->
            let contract = aux_fun_contract contract in
            trm_map aux (trm_fun ~annot:t.annot ~contract:(FunSpecContract contract) args None body)
          );
          Pattern.__ (fun () -> trm_map aux t)
        ]
       in aux t
    in
    (* let span_instrs = Mlist.map update_term span_instrs in *)
    trm_seq_helper ~annot:t_seq.annot [
      TrmMlist instrs_before;
      TrmList unfolds; TrmMlist span_instrs; TrmList folds;
      TrmMlist instrs_after]
  end

(** [split_fields]: expects the target [tg] to point at a sequence span to perform the mapping:
  - `set(base, val)` --> `set(struct_access(base, f), val.f), ...`
  - `struct_get(get(base), f)` --> `get(struct_access(base, f))`
  For now, this mapping is applied across the entire span for operations on record type [typ].

  Ghost instructions will be inserted to split the field resources around the span,
  and resources will be mapped as well:
  - `base ~> Cell` --> `struct_access(base, f) ~> Cell, ...`
  *)
let%transfo split_fields ~(typ : typvar) (tg : target) : unit =
  Resources.required_for_check ();

  let struct_def =
    match Internal.typvar_to_typedef typ with
    | Some td -> td
    | _ -> failwith "could not get the declaration of typedef for %s" (var_to_string typ)
  in
  let field_list = Internal.get_field_list struct_def in

  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    let (p_seq, span) = Path.extract_last_dir_span p in
    Target.apply_at_path (split_fields_on typ field_list span) p_seq
  ) tg)

(** [set_explicit tg]: expects the target [tg] to point at a set instruction where one struct
    instance has been assigned another struct instance. *)
let%transfo set_explicit (tg : target) : unit =
  Resources.required_for_check ();
  Nobrace_transfo.remove_after ( fun _ ->
    apply_at_target_paths (Record_core.set_explicit_on) tg)

(** [set_implicit tg]: expects the target [tg] to point at a sequence containing
      a list of struct set assignments. And transforms it into a single struct assignment.
      So it is the inverse of set_explicit. *)
let%transfo set_implicit (tg : target) : unit =
  apply_at_target_paths (Record_core.set_implicit_on) tg

(** [reorder_fields order tg]: expects the target to be pointing at typedef struct or class.
      then it changes the order of the fields based on [order].
      [order] - can be one of the following
        Move_before (x,fl) -> all the fields that belong to [fl] are moved before the field [x].
        Move_after (x,fl) -> all the fields that belong to [fl] are moved after the field [x].
        Reorder_all [fl] -> all the fields are reorder an will appear like [fl].

    @correctness: Correct if pointer arithmetic to field is replaced everywhere,
      might be impossible to prove in case of casts between types.*)
let%transfo reorder_fields (order : fields_order) (tg : target) : unit =
  apply_at_target_paths_in_seq (Record_core.reorder_fields_at order) tg

(** [reveal_field ~reparse field_to_reveal_field tg]: expects the target [tg] to point at a typedef struct,
    then it will find [field_to_reveal_field] and it's underlying type and it will
    replace [field_to_reveal_field] with a list of fields rename comming from its underlying type. *)
let%transfo reveal_field ?(reparse:bool=false) (field_to_reveal_field : field) (tg : target) : unit =
  reparse_after ~reparse
    (apply_at_target_paths_in_seq (Record_core.reveal_field_at field_to_reveal_field))
    tg

(** [reveal_fields fields_to_reveal_field tg]: an extension to the reveal_field transformation, this one
     is applied on multiple struct fields. *)
let%transfo reveal_fields ?(reparse : bool = false) (fields_to_reveal_field : fields) (tg : target) : unit =
  List.iter (fun f -> reveal_field f tg) fields_to_reveal_field


(** [to_variables tg]: expects the target [tg] to point at a variable declaration of type typedef Record.
    Then it will transform this declaration into a list of variable declarations where the type
    of these variables is inherited from the type of the struct definition. All the struct_accesses
    are going to be changed to variable occurrences. *)
let%transfo to_variables (tg : target) : unit =
  Trace.justif "correct when produced code typechecks";
  Target.iter (fun p ->
    (* FIXME: this is to remove braces before typing is triggered *)
    Nobrace_transfo.remove_after (fun () ->
      apply_at_target_paths_in_seq (Record_core.to_variables_at) (target_of_path p)
    )
  ) tg

(** [rename_fields rename tg] expects the target [tg] to point at a struct declaration,
    then it will rename all the fields that are matched when applying the type [rename]
    which can be a function to rename all the struct fields or only those that
    are matched by the [pattern] given as argument when the function [only_for] is used (see struc_core.ml). *)
let%transfo rename_fields (rename : rename) (tg : target) : unit =
  apply_at_target_paths_in_seq (fun i t -> Record_core.rename_fields_at i rename t) tg

(** [applyto_fields_type ~reparse pattern typ_update tg]: expects the target [tg] to point at a
    struct definition, then it will update all the struct field types whose identifier matches [pattern]. *)
let%transfo applyto_fields_type ?(reparse : bool = false) (pattern : string) (typ_update: typ -> typ) (tg : target) : unit =
  reparse_after ~reparse (apply_at_target_paths (Record_core.update_fields_type_on pattern typ_update)) tg

(** [update_fields_type pattern ty tg]: expects the target [tg] to point at a struct declaration,
    then it will change the current type to [ty] for all the fields that are matched by [pattern]. *)
let update_fields_type ?(reparse : bool = false) (pattern : string) (ty : typ) (tg : target) : unit =
  applyto_fields_type ~reparse pattern (fun _ -> ty) tg

(** [simpl_proj tg]: expects the target [tg] to point at any node whose descendants can contain struct
    initialization list projections. *)
let%transfo simpl_proj (tg : target) : unit =
  Resources.justif_correct "arguments are pure/reproducible";
  apply_at_target_paths (Record_core.simpl_proj_on) tg

(** [struct_modif new_fields f_get f_set use_annot_of tg]: expects the target [tg] to point at a typedef struct,
    then it will replace its current fields with [new_fields]. After modifying the fields it will search for
    accesses of the targeted struct and modify them, if they are surrounded by a set operation it will apply
    [f_set] on that access otherwise [f_get] is going to be applied. *)
let%transfo struct_modif (arg : Struct_modif.arg) (tg : target) : unit =
  apply_at_target_paths_in_seq (Record_core.struct_modif_at arg) tg

(* FUTURE
let struct_modif_simple ?(use_annot_of : bool = false) ?(new_fields : (label * typ) list = [])       ~ ?f_get:(f_get : (trm -> trm) option) ?f_set:(f_set : (trm -> trm) option) : target -> unit =
  struct_modif {
    f_get = (match f_get with Some f ->
         (fun aux t -> let t' = f aux t in
            if use_annot_of then { t' with annot = t.annot })
      | None -> (fun _ _ -> assert false));
    f_set: modif;
    f_struct_get: modif:
    f_access: modif;
 *)


(** [change_field_access_kind acc_kind f tg]: expects the target [tg] to point a typedef, then it will find
    field [f] at change its access kind to [acc_kind]. *)
let%transfo change_field_access_kind ?(field : field = "") (acc_kind : record_field_annot) (tg : target) : unit =
  apply_at_target_paths (Record_core.change_field_access_kind_on acc_kind field) tg

(** [make_all_members_public tg]: expects the target [tg] to point at a typedef struct or class.
    then it will transform all its members to public. *)
let make_all_memebers_public : target -> unit =
  change_field_access_kind Access_public

(** [method_to_const method_name]: expects the target [ŧg] to be pointing at a typedef record definition.
    Then it will check if the method of that record definition is already a const method or not.
    If it's a const method then this transformation does nothing, otherwise it will transform that method to a const one.
    Note: If [method_name] is not specified by the user all the methods will be converted to const methods.*)
let%transfo method_to_const ?(method_name : var = dummy_var) (tg : target) : unit =
  apply_at_target_paths (Record_core.method_to_const_on method_name) tg
