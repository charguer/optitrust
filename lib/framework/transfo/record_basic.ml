open Prelude
open Target
include Record_core
include Record_core.Rename

type fracs_map = resource_item Tools.String_map.t option Var_map.t ref

let split_fields_on (typvar : typvar) (field_list : (field * typ) list)
  (span : Dir.span) (t_seq : trm) : trm =
  let instrs = trm_inv ~error:"expected seq" trm_seq_inv t_seq in
  if span.start >= span.stop then begin
    t_seq
  end else begin
    let (span_instrs, instrs_after) = Mlist.split span.stop instrs in
    let (instrs_before, span_instrs) = Mlist.split span.start span_instrs in
    (* DEBUG Show.trms ~msg:"span_instrs" (Mlist.to_list span_instrs); *)
    let typ_matches typ =
      Pattern.pattern_match typ [
        Pattern.(trm_var (var_eq typvar)) (fun () -> true);
        Pattern.__ (fun () -> false)
      ]
    in
    let ptr_typ_matches typ =
      Pattern.pattern_match typ [
        Pattern.(typ_ptr !__) (fun typ () -> typ_matches typ);
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
        begin try (
          Mlist.iter (fun t ->
            let rec search t =
              (* DEBUG Printf.printf "found: %s\n" Ast_to_text.(ast_to_string ~style:style_types t); *)
              match t.typ with
              | Some typ when matches_base t -> raise (TypeFound typ)
              | _ -> trm_map search t
            in
            ignore (search t)
          ) span_instrs;
          failwith "could not find type of base '%s'" (Ast_to_c.ast_to_string base_pattern)
        ) with
        | TypeFound typ -> ptr_typ_matches typ
        end
      | Some typ -> ptr_typ_matches typ
    in
    let process_matching_resource_item process_one_cell default (h, formula) =
      let open Resource_formula in
      let (mode, inner_formula) = formula_mode_inv formula in
      (* DEBUG Printf.printf "R: %s\n" (Resource_computation.formula_to_string formula); *)
      let rec aux wrap_cell formula =
        Pattern.pattern_match formula [
          Pattern.(formula_group !__ (trm_fun (pair !__ __ ^:: nil) !__ !__ __)) (fun range idx _frettyp body_formula () ->
            aux (fun c -> wrap_cell (trm_apps ~annot:formula.annot trm_group [range; formula_fun [idx, typ_int] None c])) body_formula
          );
          Pattern.(formula_model !__ (trm_var (var_eq var_cell))) (fun loc () ->
            Pattern.when_ (trm_ptr_typ_matches loc);
            (* DEBUG Printf.printf "MATCH!\n"; *)
            Some [process_one_cell (fun c -> trm_copy (wrap_cell c)) mode loc]
          );
          Pattern.__ (fun () -> None)
        ]
      in
      match aux (fun c -> c) inner_formula with
      | Some f -> f
      | None -> default ()
    in
    let open Resource_formula in
    let open Resource_contract in
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
        (* TODO: think more about fractions, use same fracs_map as for ROs in contracts?
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
            (model (trm_struct_access ~field_typ:ty loc sf)))
        ) field_list in
        fold_or_unfold ~fold [frac_ghost] folded_linear unfolded_linear
      | Uninit ->
        let folded_linear = [(
          new_anon_hyp (), formula_uninit (model loc)
        )] in
        let unfolded_linear = List.map (fun (sf, ty) ->
          (new_anon_hyp (), formula_uninit (model
            (trm_struct_access ~field_typ:ty loc sf)
          ))
        ) field_list in
        fold_or_unfold ~fold [] folded_linear unfolded_linear
      | Full ->
        let folded_linear = [(
          new_anon_hyp (), model loc
        )] in
        let unfolded_linear = List.map (fun (sf, ty) ->
          (new_anon_hyp (), model
            (trm_struct_access ~field_typ:ty loc sf)
          )
        ) field_list in
        fold_or_unfold ~fold [] folded_linear unfolded_linear
    in
    let process_one_item ~(fold : bool) =
      process_matching_resource_item (fun wrap mode c -> process_one_cell ~fold wrap (mode, c)) (fun () -> [])
    in
    let (unfolds, folds) = if !Flags.check_validity then begin
      let first = Option.unsome ~error:"expected first instruction" (Mlist.nth_opt span_instrs 0) in
      let last = Option.unsome ~error:"expected last instruction" (Mlist.lst span_instrs) in
      let res_start = Resources.before_trm first in
      let res_stop = Resources.after_trm last in
      let unfolds = List.concat_map (process_one_item ~fold:false) res_start.linear in
      let folds = List.concat_map (process_one_item ~fold:true) res_stop.linear in
      (unfolds, folds)
    end else begin
      ([], [])
    end in
    let update_term t =
      let open Tools in
      (* fracs_map : maps bound fractions to splitted bound fractions. *)
      let fracs_map_init pure_res : fracs_map =
        ref (List.fold_left (fun acc (h, pure_f) ->
          Pattern.pattern_match pure_f [
            Pattern.(trm_var (var_eq var_frac)) (fun () ->
              Var_map.add h None acc
            );
            Pattern.__ (fun () -> acc)
          ]
        ) Var_map.empty pure_res)
      in
      let fracs_map_update_fracs (fracs_map : fracs_map) pure_res =
        List.concat_map (fun (h, pure_f) ->
          match Var_map.find_opt h !fracs_map with
          | None | Some None -> [(h, pure_f)]
          | Some (Some hs) -> List.map snd (String_map.bindings hs)
        ) pure_res
      in
      let fracs_map_split_frac_var (fracs_map : fracs_map) sf field_list frac_var =
        match Var_map.find_opt frac_var !fracs_map with
        | None -> frac_var
        | Some (Some hs) -> fst (String_map.find sf hs)
        | Some None ->
          let hs = List.fold_left (fun acc (sf, _) ->
            String_map.add sf (snd (new_frac ())) acc
          ) String_map.empty field_list in
          fracs_map := Var_map.add frac_var (Some hs) !fracs_map;
          fst (String_map.find sf hs)
      in
      let fracs_map_split_frac (fracs_map : fracs_map) sf field_list frac =
        let rec aux frac =
          Pattern.pattern_match frac [
            Pattern.(trm_var !__) (fun frac_var () ->
              trm_var ?typ:frac.typ (fracs_map_split_frac_var fracs_map sf field_list frac_var)
            );
            Pattern.__ (fun () -> trm_map aux frac)
          ]
        in aux frac
      in
      let aux_resource_items (fracs_map : fracs_map) items =
        List.concat (List.concat_map (fun (h, formula) ->
          process_matching_resource_item (fun wrap _mode loc ->
            (* DEBUG Printf.printf "formula:%s\n" (Ast_to_c.ast_to_string ~style formula); *)
            List.map (fun (sf, ty) ->
              let new_formula = match formula_read_only_inv formula with
              | Some { frac; formula } ->
                let new_frac = fracs_map_split_frac fracs_map sf field_list frac in
                formula_read_only ~frac:new_frac (wrap (formula_model (trm_struct_access ~field_typ:ty loc sf) trm_cell))
              | None -> formula_map_under_mode (fun _ ->
                wrap (formula_model (trm_struct_access ~field_typ:ty loc sf) trm_cell)
              ) formula
              in
              (new_anon_hyp (), new_formula)
            ) field_list
          ) (fun () -> [[(h, formula)]]) (h, formula)
        ) items)
      in
      let aux_resource_set fracs_map res =
        { res with
          pure = aux_resource_items fracs_map res.pure;
          linear = aux_resource_items fracs_map res.linear }
      in
      let aux_fun_contract fracs_map contract =
        { pre = aux_resource_set fracs_map contract.pre;
          post = aux_resource_set fracs_map contract.post }
      in
      let unfold_alloc t =
        let res = Resources.after_trm t in
        let usage = Resources.usage_of_trm t in
        let produced = List.filter (Resource_set.(linear_usage_filter usage keep_produced)) res.linear in
        let unfolds = List.concat_map (process_one_item ~fold:false) produced in
        trm_seq_nobrace_nomarks ([t] @ unfolds)
      in
      let fold_free t =
        let res = Resources.before_trm t in
        let usage = Resources.usage_of_trm t in
        let consumed = List.filter (Resource_set.(linear_usage_filter usage keep_used)) res.linear in
        let folds = List.concat_map (process_one_item ~fold:true) consumed in
        trm_seq_nobrace_nomarks (folds @ [t])
      in
      (* FIXME: duplicated code with set_explicit *)
      let check_pure = if !Flags.check_validity then (fun name x ->
        if Resources.trm_is_pure x then Trace.justif (sprintf "duplicated %s is pure" name)
      ) else (fun name x ->
        ()
      ) in
      let rec aux (t : trm) : trm =
        Pattern.pattern_match t [
          Pattern.(trm_let !__ !__ !(trm_ref __ !__)) (fun v typ ref init () ->
            Pattern.when_ (ptr_typ_matches typ);
            unfold_alloc t
          );
          Pattern.__ (fun () ->
            match Matrix_core.let_alloc_inv_with_ty t with
            | Some (v, dims, typ, size) ->
              Pattern.when_ (typ_matches typ);
              unfold_alloc t
            | None -> raise Pattern.Next
          );
          Pattern.__ (fun () ->
            match Matrix_trm.free_inv t with
            | Some to_free ->
              Pattern.when_ (trm_ptr_typ_matches to_free);
              fold_free t
            | None -> raise Pattern.Next
          );
          Pattern.(trm_set !__ (trm_get !__)) (fun base get_base () ->
            (* NOTE: base and get_base are pure as they appear in contracts. *)
            Pattern.when_ (trm_ptr_typ_matches base);
            let set_one (sf, ty) =
              trm_set (trm_struct_access ~field_typ:ty base sf)
                (trm_get (trm_struct_access ~field_typ:ty get_base sf))
            in
            trm_seq_nobrace_nomarks (List.map set_one field_list)
          );
          Pattern.(trm_set !__ (trm_record !__)) (fun base (_, fs) () ->
            Pattern.when_ (trm_ptr_typ_matches base);
            Printf.printf "%s\n" Ast_to_text.(ast_to_string ~style:style_types t);
            let st = List.split_pairs_snd (Mlist.to_list fs) in
            let set_one i (sf, ty) =
              trm_set (trm_struct_access ~field_typ:ty base sf) (List.nth st i)
            in
            trm_seq_nobrace_nomarks (List.mapi set_one field_list)
          );
          Pattern.(trm_set !__ !__) (fun base value () ->
            Printf.printf "%s\n" Ast_to_text.(ast_to_string ~style:style_types t);
            Pattern.when_ (trm_ptr_typ_matches base);
            check_pure "set value" value;
            let set_one (sf, ty) =
              trm_set (trm_struct_access ~field_typ:ty base sf) (trm_struct_get ~field_typ:ty value sf)
            in
            trm_seq_nobrace_nomarks (List.map set_one field_list)
          );
          Pattern.(trm_struct_get (trm_get !__) !__) (fun base field () ->
            Pattern.when_ (trm_ptr_typ_matches base);
            trm_get (trm_struct_access base field)
          );
          (* TODO: also do other contracts *)
          Pattern.(trm_for !__ !__ !__) (fun range body spec () ->
            let fracs_map = fracs_map_init spec.loop_ghosts in
            let contract = { spec with
              invariant = aux_resource_set fracs_map spec.invariant;
              parallel_reads = aux_resource_items fracs_map spec.parallel_reads;
              iter_contract = aux_fun_contract fracs_map spec.iter_contract;
            } in
            let contract = { contract with
              loop_ghosts = fracs_map_update_fracs fracs_map spec.loop_ghosts;
            } in
            trm_map aux (trm_for ~annot:t.annot ~contract range body)
          );
          Pattern.(trm_fun_with_contract !__ !__ !__) (fun args body contract () ->
            let fracs_map = fracs_map_init contract.pre.pure in
            let contract = aux_fun_contract fracs_map contract in
            let contract = { contract with pre = { contract.pre with pure =
              fracs_map_update_fracs fracs_map contract.pre.pure
            }} in
            trm_map aux (trm_fun ~annot:t.annot ~contract:(FunSpecContract contract) args None body)
          );
          Pattern.__ (fun () -> trm_map aux t)
        ]
       in aux t
    in
    let span_instrs = Mlist.map update_term span_instrs in
    trm_seq_helper ~annot:t_seq.annot [
      TrmMlist instrs_before;
      TrmList unfolds; TrmMlist span_instrs; TrmList folds;
      TrmMlist instrs_after]
  end

(** [split_fields]: expects the target [tg] to point at a sequence span to perform the mapping:
  - `set(base, get(get_base))` --> `set(struct_access(base, f), get(struct_access(get_base, f))), ...`
  - `set(base, { .f = v; .. })` --> `set(struct_access(base, f) = v`
  - `set(base, val)` --> `set(struct_access(base, f), struct_get(val, f)), ...`
  - `struct_get(get(base), f)` --> `get(struct_access(base, f))`
  For now, this mapping is applied across the entire span for operations on record type [typ].

  Ghost instructions will be inserted to split the field resources in the span,
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

  if !Flags.check_validity then
    Trace.justif "correct if the produced code typechecks";

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
