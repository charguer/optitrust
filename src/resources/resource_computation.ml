open Ast
open Trm
open Typ
open Resource_formula
open Resource_contract

type pure_resource_set = resource_item list
type linear_resource_set = resource_item list

(** Maps OptiTrust primitives to variables that can be given a contract in [optitrust.h] *)
module Resource_primitives = struct
  let __cast = toplevel_var "__cast"
  let __new = toplevel_var "__new"
  let __get = toplevel_var "__get"
  let __set = toplevel_var "__set"
  let __add = toplevel_var "__add"
  let __sub = toplevel_var "__sub"
  let __mul = toplevel_var "__mul"
  let __array_access = toplevel_var "__array_access"
  let __add_inplace = toplevel_var "__add_inplace"
  let __sub_inplace = toplevel_var "__sub_inplace"
  let __mul_inplace = toplevel_var "__mul_inplace"
  let __post_inc = toplevel_var "__post_inc"
  let __post_dec = toplevel_var "__post_dec"
  let __pre_inc = toplevel_var "__pre_inc"
  let __pre_dec = toplevel_var "__pre_dec"

  exception Unknown

  let unop_to_var_name (u: unary_op): string =
    match u with
    | Unop_get -> "__get"
    | Unop_pre_inc -> "__pre_inc"
    | Unop_pre_dec -> "__pre_dec"
    | Unop_post_inc -> "__post_inc"
    | Unop_post_dec -> "__post_dec"
    | Unop_cast t -> "__cast"
    | _ -> raise Unknown

  let binop_to_var_name (u: binary_op): string =
    match u with
    | Binop_add -> "__add"
    | Binop_sub -> "__sub"
    | Binop_mul -> "__mul"
    | Binop_array_access -> "__array_access"
    | Binop_set -> "__set"
    | _ -> raise Unknown

  let to_var (p: prim): var =
    toplevel_var (match p with
    | Prim_unop u -> unop_to_var_name u
    | Prim_binop b -> binop_to_var_name b
    | Prim_compound_assgn_op b -> (binop_to_var_name b ^ "_inplace")
    | Prim_new _ -> "__new"
    | _ -> raise Unknown)

end

let var_result = Resource_set.var_result

(**
   When instantating linear resources in a contract with X := Y substitutions, Y is a [Formula_inst.t].

   These formulas do not necessarily have stable var-ids.
  *)
module Formula_inst = struct
  type t = formula

  let inst_hyp (h: hyp): t = trm_var h
  let inst_hyp_inv = trm_var_inv

  let var_SplitRO = toplevel_var "SplitRO"

  let inst_split_read_only ~(new_frac: var) (h: hyp) : t =
    trm_apps (trm_var var_SplitRO) [trm_var new_frac; inst_hyp h]

  let inst_split_read_only_inv (f: t): (var * hyp) option =
    Pattern.pattern_match_opt f [
      Pattern.(trm_apps2 (trm_var (var_eq var_SplitRO)) (trm_var !__) (trm_var !__)) (fun frac hyp -> (frac, hyp))
    ]

  let var_ForgetInit = toplevel_var "ForgetInit"

  let inst_forget_init (h: hyp) : t =
    trm_apps (trm_var var_ForgetInit) [inst_hyp h]

  let inst_forget_init_inv (f: t): hyp option =
    Pattern.pattern_match_opt f [
      Pattern.(trm_apps1 (trm_var (var_eq var_ForgetInit)) (trm_var !__)) (fun h -> h)
    ]
end

(** [Resource_not_found (item, res_list)]: exception raised when the resource
   [item] is not found inside the resource list [res_list] *)
exception Resource_not_found of resource_item * resource_item list

let raise_resource_not_found ((name, formula): resource_item) (evar_ctx: unification_ctx) (inside: resource_item list) =
  let subst_ctx = Var_map.mapi (fun var subst ->
    match subst with Some t -> t | None -> trm_var { var with name = ("?" ^ var.name) }
  ) evar_ctx in
  let formula = trm_subst subst_ctx formula in
  raise (Resource_not_found ((name, formula), inside))

(** [unify_pure (x, formula) res evar_ctx] unifies the given [formula] with one of the resources in [res].

   Also add a binding from [x] to the found resource in [evar_ctx].
   If it fails raise a {!Resource_not_found} exception. *)
let unify_pure ((x, formula): resource_item) (res: pure_resource_set) (evar_ctx: unification_ctx): unification_ctx =
  (* Add flag to disallow pure instantiation *)
  let find_formula formula (hyp_candidate, formula_candidate) =
    Option.map (fun evar_ctx -> Var_map.add x (Some (trm_var hyp_candidate)) evar_ctx)
      (unify_trm formula_candidate formula evar_ctx)
  in
  match List.find_map (find_formula formula) res with
  | Some evar_ctx -> evar_ctx
  | None -> raise_resource_not_found (x, formula) evar_ctx res


(** [subtract_linear_resource_item ~split_frac (x, formula) res evar_ctx] subtracts [formula]
  from [res].
  Returns [(used, res', evar_ctx')] where:
  - [used] is the consumed resource item
  - [res'] is what remains from [res]
  - [evar_ctx'] is the evar context updated with new unification choices

  If [split_frac = true], then substracting [RO(?, R)] will only consume a fraction [g] of an
  [RO(f, R)] from [res]: [RO(g, R)] is consumed and [RO(f - g, R)] still remains.

  Depending on the formula of the resource, decide if we need a read-only fraction,
  a potentially uninitialized resource or a full ownership.

  Raises {!Resource_not_found} if the [formula] cannot be consumed.
   *)
let subtract_linear_resource_item ~(split_frac: bool) ((x, formula): resource_item)
  (res: linear_resource_set) (evar_ctx: unification_ctx)
  : used_resource_item * linear_resource_set * unification_ctx =
  let open Xoption.OptionMonad in

  let rec extract
    (f : resource_item -> (used_resource_item * resource_item option * unification_ctx) option)
    (res: linear_resource_set)
    (evar_ctx: unification_ctx): used_resource_item * linear_resource_set * unification_ctx =
    match res with
    | [] -> raise Not_found (* caught later to create a Resource_not_found. *)
    | candidate :: res ->
      begin match f candidate with
      | Some (used_res, Some leftover, evar_ctx) -> (used_res, leftover :: res, evar_ctx)
      | Some (used_res, None, evar_ctx) -> (used_res, res, evar_ctx)
      | None ->
        let used, res, evar_ctx = extract f res evar_ctx in
        (used, candidate :: res, evar_ctx)
      end
  in

  let unify_and_remove_linear
    ((x, formula): resource_item)
    ~(uninit : bool) (* was [formula] surrounded by Uninit? *)
    (res: linear_resource_set)
    (evar_ctx: unification_ctx): used_resource_item * linear_resource_set * unification_ctx =
    (* Used by {!subtract_linear_resource_item} in the case where [formula] is not a read-only resource. *)
    (* LATER: Improve the structure of the linear_resource_set to make this
      function faster on most frequent cases *)
    extract (fun (candidate_name, formula_candidate) ->
      let used_formula, formula_to_unify =
        if uninit then
          match formula_uninit_inv formula_candidate with
          (* standard case: Uninit consumes Uninit *)
          | Some formula_candidate -> Formula_inst.inst_hyp candidate_name, formula_candidate
          (* coercion case: Uninit consumes Full *)
          | None -> Formula_inst.inst_forget_init candidate_name, formula_candidate
        else Formula_inst.inst_hyp candidate_name, formula_candidate
      in
      let* evar_ctx = unify_trm formula_to_unify formula evar_ctx in
      let used_formula = if uninit then formula_uninit formula_to_unify else formula_to_unify in
      Some (
        { pre_hyp = x; inst_by = Formula_inst.inst_hyp candidate_name; used_formula },
        None,
        evar_ctx)
    ) res evar_ctx
  in

  let unify_and_split_read_only
    (pre_hyp: hyp) ~(new_frac: var) (formula: formula)
    (res: linear_resource_set)
    (evar_ctx: unification_ctx): used_resource_item * linear_resource_set * unification_ctx =
    (* Used by {!subtract_linear_resource_item} in the case where [formula] is a read-only resource. *)
    (* LATER: Improve the structure of the linear_resource_set to make this
      function faster on most frequent cases *)
    extract (fun (h, formula_candidate) ->
      let cur_frac, formula_candidate = match formula_read_only_inv formula_candidate with
        | Some { frac; formula } -> frac, formula
        | None -> full_frac, formula_candidate
      in
      let* evar_ctx = unify_trm formula_candidate formula evar_ctx in
      Some (
        { pre_hyp ; inst_by = Formula_inst.inst_split_read_only ~new_frac h; used_formula = formula_read_only ~frac:(trm_var new_frac) formula_candidate },
        Some (h, formula_read_only ~frac:(trm_sub cur_frac (trm_var new_frac)) formula_candidate), evar_ctx)
    ) res evar_ctx
  in

  try
    Pattern.pattern_match formula [
      (* special case where _Full disables split_frac. *)
      Pattern.(formula_read_only (trm_apps1 (trm_var (var_eq _Full)) !__) !__) (fun frac ro_formula ->
        unify_and_remove_linear (x, formula_read_only ~frac ro_formula) ~uninit:false res evar_ctx
      );
      (* we split a fraction from an RO if we don't care about the fraction we get (evar). *)
      Pattern.(formula_read_only (trm_var !__) !__) (fun frac_var ro_formula ->
        Pattern.when_ (split_frac && Var_map.find_opt frac_var evar_ctx = Some None);
        (* TODO: ADT == Some NotKnown *)
        let new_frac, _ = new_frac () in
        let evar_ctx = Var_map.add frac_var (Some (trm_var new_frac)) evar_ctx in
        unify_and_split_read_only x ~new_frac ro_formula res evar_ctx
      );
      Pattern.(formula_uninit !__) (fun formula ->
        unify_and_remove_linear (x, formula) ~uninit:true res evar_ctx);
      Pattern.(!__) (fun _ ->
        unify_and_remove_linear (x, formula) ~uninit:false res evar_ctx)
    ]
  with Not_found ->
    raise_resource_not_found (x, formula) evar_ctx res

(** [subtract_linear_resource_set ?split_frac ?evar_ctx res_from res_removed] subtracts [res_removed] from [res_from].
  Returns [(used, res', evar_ctx')] where:
  - [used] are the consumed resource items
  - [res'] is what remains from [res]
  - [evar_ctx'] is the evar context updated with new unification choices

  Raise [Resource_not_found] if one resource is missing.
  Use the unification environment [evar_ctx] for all resources in [res_removed]
  potentially instantiating evars inside.
  If [split_frac] is true, always try to give a smaller fraction than what is
  inside [res_from] for evar fractions.
*)
let subtract_linear_resource_set ?(split_frac: bool = false) ?(evar_ctx: unification_ctx = Var_map.empty)  (res_from: linear_resource_set) (res_removed: linear_resource_set)
  : used_resource_item list * linear_resource_set * unification_ctx =
  List.fold_left (fun (used_list, res_from, evar_ctx) res_item ->
    let used, res_from, evar_ctx = subtract_linear_resource_item ~split_frac res_item res_from evar_ctx in
    (used :: used_list, res_from, evar_ctx)
  ) ([], res_from, evar_ctx) res_removed

(** [eliminate_dominated_evars res] eliminates dominated evars from [res].
  Returns the the remaining pure resources and the dominated evars.

  An evar is dominated if its value will be implied by the instantation of other resources
  (i.e. it appears in the formula of another resource). *)
let eliminate_dominated_evars (res: resource_set): pure_resource_set * var list =
  (* TODO: maybe check free vars inside function contracts? *)
  (* This function completely forgets about ghost variable shadowing *)
  (* LATER: Un tri topologique serait un peu plus robuste *)
  let combine_used_vars used_vars (_, formula) =
    Var_set.union used_vars (trm_free_vars formula)
  in
  let used_vars = List.fold_left combine_used_vars Var_set.empty res.pure in
  let used_vars = List.fold_left combine_used_vars used_vars res.linear in
  let dominated_evars = ref [] in
  let pure = List.filter (fun (h, _) ->
      if Var_set.mem h used_vars then
        (dominated_evars := h :: !dominated_evars; false)
      else true) res.pure
  in
  (pure, !dominated_evars)

exception Spec_not_found of var
exception Anonymous_function_without_spec
exception NotConsumedResources of linear_resource_set
exception ImpureFunctionArgument of exn

(** [extract_resources ~split_frac res_from subst_ctx res_to] checks that
  [res_from ==> res_to * H]
  in separation logic.

  Returns the leftover linear resources [H] along with the substitution context after
  instantiating ghost variables in [res_to].
  Effectively, this checks that all resources inside [res_to] can be built from resources inside
  [res_from] and returns the remaining linear resources after instantiation.
  Pure resources (ghosts) can be inferred using unification.

  If given, [subst_ctx] substitutes variables in [res_to].
  Instantiated pure resources must not be bound inside [res_to].

  TODO: Add unit tests for this specific function
*)
let extract_resources ~(split_frac: bool) (res_from: resource_set) ?(subst_ctx: tmap = Var_map.empty) (res_to: resource_set) : tmap * used_resource_set * linear_resource_set =
  let not_dominated_pure_res_to, dominated_evars = eliminate_dominated_evars res_to in
  let evar_ctx = Var_map.map (fun x -> Some (trm_subst res_from.aliases x)) subst_ctx in
  let evar_ctx = List.fold_left (fun evar_ctx x -> Var_map.add x None evar_ctx) evar_ctx dominated_evars in

  let res_from = Resource_set.subst_all_aliases res_from in
  assert (Var_map.is_empty res_to.aliases);

  let used_linear, leftover_linear, evar_ctx = subtract_linear_resource_set ~split_frac ~evar_ctx res_from.linear res_to.linear in
  let evar_ctx = List.fold_left (fun evar_ctx res_item ->
      unify_pure res_item res_from.pure evar_ctx) evar_ctx not_dominated_pure_res_to
  in

  (* All dominated evars should have been instantiated at this point.
     There is a bug if it's not the case. *)
  let subst_ctx = Var_map.map (function
      | Some t -> t
      | None -> failwith "failed to instantiate evars") evar_ctx
  in

  let used_pure = List.map (fun (hyp, formula) ->
      { pre_hyp = hyp; inst_by = Var_map.find hyp subst_ctx; used_formula = trm_subst subst_ctx formula }
    ) res_to.pure in

  (* Check higher order function contracts in the post-condition *)
  ignore (Var_map.merge
            (fun fn_name spec_from spec_to ->
              match spec_from, spec_to with
              | _, None -> None (* We can drop extra specifications *)
              | None, Some _ -> raise (Spec_not_found fn_name)
              | Some spec_from, Some spec_to ->
                failwith (sprintf "higher order functions are not yet implemented (found a spec for %s in pre-condition)" (var_to_string fn_name))
            )
            res_from.fun_specs res_to.fun_specs);

  (subst_ctx, { used_pure; used_linear }, leftover_linear)

(* FIXME: resource set intuition breaks down, should we talk about resource predicates? *)
(** [assert_resource_impl res_from res_to] checks that [res_from] ==> [res_to]. *)
let assert_resource_impl (res_from: resource_set) (res_to: resource_set) : used_resource_set =
  let _, used_res, leftovers = extract_resources ~split_frac:false res_from res_to in
  if leftovers <> [] then raise (NotConsumedResources leftovers);
  used_res

(** [compute_produced_resources subst_ctx contract_post] returns the resources produced
    by [contract_post] given the [subst_ctx] instantation of the contract. *)
let compute_produced_resources (subst_ctx: tmap) (contract_post: resource_set)
  : produced_resource_set =
  let compute_produced_resources_list =
    List.fold_left_map (fun subst_ctx (h, formula) ->
        let produced_hyp = new_anon_hyp () in
        let produced_formula = trm_subst subst_ctx formula in
        let produced = { produced_hyp; post_hyp = h; produced_formula } in
        let subst_ctx = Var_map.add h (trm_var produced_hyp) subst_ctx in
        (subst_ctx, produced)
      )
  in
  let subst_ctx, produced_pure = compute_produced_resources_list subst_ctx contract_post.pure in
  let _, produced_linear = compute_produced_resources_list subst_ctx contract_post.linear in
  { produced_pure; produced_linear }

let produced_resources_to_resource_set (res_produced: produced_resource_set): resource_set =
  let forget_origin =
    List.map (fun { produced_hyp; produced_formula } -> (produced_hyp, produced_formula))
  in
  let pure = forget_origin res_produced.produced_pure in
  let linear = forget_origin res_produced.produced_linear in
  Resource_set.make ~pure ~linear ()


(* [resource_merge_after_frame]:
 * Returns [res_after] * [frame] with simplifications.
 * Cancels magic wands in [frame] with linear resources in [res_after] and
 * returns the produced resource_set.
 *
 * Ex: res_after.linear = RO('a, t)  and  frame = RO('b - 'a, t)
 * gives res.linear = RO('b, t)
 *)
let resource_merge_after_frame (res_after: produced_resource_set) (frame: linear_resource_set) : resource_set * resource_usage_map =
  let res_after = produced_resources_to_resource_set res_after in
  let ro_formulas = Hashtbl.create (List.length res_after.linear) in
  (* Accumulate into ro_formulas the pairs ('a, t) when res_after.linear contains RO('a, t) *)
  List.iter (fun (_, formula) ->
      match formula_read_only_inv formula with
      | Some { frac; formula = ro_formula } ->
        begin match trm_var_inv frac with
        | Some frac_var -> Hashtbl.add ro_formulas frac_var.id ro_formula
        | None -> ()
        end
      | None -> ()
    ) res_after.linear;

  let rec try_pop_ro_formula frac ro_formula =
    match Hashtbl.find_opt ro_formulas frac.id with
    | Some candidate_formula ->
      Hashtbl.remove ro_formulas frac.id;
      if are_same_trm ro_formula candidate_formula then
        true
      else
        let res = try_pop_ro_formula frac ro_formula in
        Hashtbl.add ro_formulas frac.id candidate_formula;
        res
    | None -> false
  in
  let rec reunite_fracs frac ro_formula =
    match trm_binop_inv Binop_sub frac with
    | Some (sub_frac, frac_atom) ->
      let sub_frac' = reunite_fracs sub_frac ro_formula in
      begin match trm_var_inv frac_atom with
      | Some atom when try_pop_ro_formula atom ro_formula -> sub_frac'
      (* DEBUG:
      | Some atom ->
        Printf.eprintf "Failed to find %s\n" (var_to_string atom);
        Printf.eprintf "%s\n" (AstC_to_c.ast_to_string ro_formula);
        Hashtbl.iter (fun (a, _) () -> Printf.eprintf "%b -- %s\n" (a = ro_formula) (AstC_to_c.ast_to_string a)) ro_formulas;
        trm_sub sub_frac frac_atom *)
      | _ -> if sub_frac == sub_frac' then frac else trm_sub sub_frac' frac_atom
      end
    | None -> frac
  in
  let used_set, frame = List.fold_left_map (fun used_set (x, formula) ->
      match formula_read_only_inv formula with
      | Some { frac = old_frac; formula = ro_formula } ->
        let frac = reunite_fracs old_frac ro_formula in
        if frac == old_frac then
          (used_set, (x, formula))
        else
          let used_set = Hyp_map.add x JoinedReadOnly used_set in
          let res = match frac.desc with
            | Trm_val Val_lit Lit_int 1 -> (x, ro_formula)
            | _ -> (x, formula_read_only ~frac ro_formula)
          in
          used_set, res
      | None -> (used_set, (x, formula))
    ) Hyp_map.empty frame
  in

  let linear, used_set = List.fold_left (fun (acc, used_set) (h, formula) ->
      let add_produced () = ((h, formula) :: acc, Var_map.add h Produced used_set) in
      let skip () = (acc, used_set) in
      match formula_read_only_inv formula with
      | Some { frac; formula = ro_formula } ->
        (* DEBUG
        Hashtbl.iter (fun (a, _) () -> Printf.eprintf "%b -- %s\n" (a = ro_formula_erased) (AstC_to_c.ast_to_string a)) ro_formulas;
        Printf.eprintf "formula: %s\n" (AstC_to_c.ast_to_string ro_formula_erased); *)
        begin match trm_var_inv frac with
        | Some frac when try_pop_ro_formula frac ro_formula -> add_produced ()
        | Some frac -> skip () (* Consumed ro_formula *)
        | None -> add_produced ()
        end
      | None -> add_produced ())
    (frame, used_set) res_after.linear
  in
  assert (Hashtbl.length ro_formulas = 0);
  { res_after with linear }, used_set

let trm_fun_var_inv (t:trm): var option =
  try
    match trm_prim_inv t with
    | Some p -> Some (Resource_primitives.to_var p)
    | None -> trm_var_inv t
  with Resource_primitives.Unknown ->
    let trm_internal (msg : string) (t : trm) : string =
      Printf.sprintf "%s: %s\n" msg (Ast_to_text.ast_to_string t) in
    trm_fail t (trm_internal "unimplemented trm_fun_var_inv construction" t)

let find_fun_spec (t: trm) (fun_specs: fun_spec_resource varmap): fun_spec_resource =
  (* LATER: replace with a query on _Res inside fun_specs, but this requires a management of value aliases *)
  match trm_fun_var_inv t with
  | Some fn ->
    begin match Var_map.find_opt fn fun_specs with
    | Some contract -> contract
    | None -> raise (Spec_not_found fn)
    end
  | None ->
    begin match trm_inv ~error:"expected anonymous or named function" trm_fun_inv t with
    | (args, _, _, FunSpecContract contract) ->
      { args = List.map fst args; contract; inverse = None }
    | _ -> raise Anonymous_function_without_spec
    end

let resource_list_to_string res_list : string =
  Ast_fromto_AstC.ctx_resource_list_to_string ~sep:"\n" res_list

let resources_to_string res : string =
  match res with
  | Some res ->
  let spure = resource_list_to_string res.pure in
  let slin = resource_list_to_string res.linear in
  Printf.sprintf "pure:\n%s\n\nlinear:\n%s\n" spure slin
  | None -> "UnspecifiedRes"

type resource_error_phase = ResourceComputation | ResourceCheck
exception ResourceError of location * resource_error_phase * exn

let _ = Printexc.register_printer (function
  | Resource_not_found (item, context) ->
    Some (Printf.sprintf "Resource not found:\n%s\nIn context:\n%s" (Ast_fromto_AstC.named_formula_to_string item) (resource_list_to_string context))
  | Spec_not_found fn ->
    Some (Printf.sprintf "No specification for function %s" (var_to_string fn))
  | NotConsumedResources res ->
    Some (Printf.sprintf "Resources not consumed after the end of the block:\n%s" (resource_list_to_string res))
  | ImpureFunctionArgument err ->
    Some (Printf.sprintf "Function argument subexpression resource preservation check failed: %s" (Printexc.to_string err))
  | ResourceError (loc, ResourceComputation, err) ->
    Some (Printf.sprintf "%s: Resource computation error: %s" (loc_to_string loc) (Printexc.to_string err));
  | ResourceError (loc, ResourceCheck, err) ->
    Some (Printf.sprintf "%s: Resource check error: %s" (loc_to_string loc) (Printexc.to_string err))
  | _ -> None)

let update_usage_map ~(current_usage: resource_usage_map) ~(extra_usage: resource_usage_map): resource_usage_map =
  Hyp_map.merge (fun hyp cur_use new_use ->
      match cur_use, new_use with
      | None, u -> u
      | u, None -> u
      | Some Produced, Some (UsedFull | UsedUninit) -> None
      | Some Produced, Some (SplittedReadOnly | JoinedReadOnly) -> Some Produced
      | _, Some Produced -> failwith (sprintf "Produced resource %s share id with another one" (var_to_string hyp))
      | Some (UsedFull | UsedUninit), _ -> failwith (sprintf "Consumed resource %s share id with another one" (var_to_string hyp))
      | Some (SplittedReadOnly | JoinedReadOnly), Some (UsedFull | UsedUninit) -> Some UsedFull
      | Some JoinedReadOnly, Some JoinedReadOnly -> Some JoinedReadOnly
      | Some (SplittedReadOnly | JoinedReadOnly), Some (SplittedReadOnly | JoinedReadOnly) -> Some SplittedReadOnly
    ) current_usage extra_usage

let update_usage_map_opt ~(current_usage: resource_usage_map option) ~(extra_usage: resource_usage_map option): resource_usage_map option =
  let open Xoption.OptionMonad in
  let* current_usage in
  let* extra_usage in
  Some (update_usage_map ~current_usage ~extra_usage)

let add_used_set_to_usage_map (res_used: used_resource_set) (usage_map: resource_usage_map) : resource_usage_map =
  (* TODO: Maybe manage pure as well *)
  let locally_used =
    List.fold_left (fun usage_map { inst_by; used_formula } ->
      match Formula_inst.inst_split_read_only_inv inst_by with
      | Some (_, orig_hyp) -> Hyp_map.add orig_hyp SplittedReadOnly usage_map
      | None ->
        let hyp_usage = if formula_uninit_inv used_formula = None then UsedFull else UsedUninit in
        match Formula_inst.inst_hyp_inv inst_by with
        | Some hyp -> Hyp_map.add hyp hyp_usage usage_map
        | None ->
          match Formula_inst.inst_forget_init_inv inst_by with
          | Some hyp -> Hyp_map.add hyp hyp_usage usage_map
          | None -> failwith "Weird resource used"
    ) Hyp_map.empty res_used.used_linear
  in
  update_usage_map ~current_usage:usage_map ~extra_usage:locally_used

let compute_contract_invoc (contract: fun_contract) ?(subst_ctx: tmap = Var_map.empty) (res: resource_set) (t: trm): resource_usage_map * resource_set =
    let subst_ctx, res_used, res_frame = extract_resources ~split_frac:true ~subst_ctx res contract.pre in

    let res_produced = compute_produced_resources subst_ctx contract.post in

    t.ctx.ctx_resources_contract_invoc <- Some {
      contract_frame = res_frame;
      contract_inst = res_used;
      contract_produced = res_produced };

    let usage_map = add_used_set_to_usage_map res_used Var_map.empty in

    let new_res, used_wands = resource_merge_after_frame res_produced res_frame in
    let usage_map = update_usage_map ~current_usage:usage_map ~extra_usage:used_wands in
    usage_map, Resource_set.bind ~old_res:res ~new_res

let debug_print_computation_stack = false

let handle_resource_errors (loc: location) (exn: exn) =
  match exn with
  | e when !Flags.resource_errors_as_warnings ->
    Printf.eprintf "%s: Resource computation warning: %s\n" (loc_to_string loc) (Printexc.to_string e);
    None, None
  | ResourceError (None, place, err) -> Printexc.(raise_with_backtrace (ResourceError (loc, place, err)) (get_raw_backtrace ()))
  | ResourceError (Some _, _, _) as e -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
  | e -> Printexc.(raise_with_backtrace (ResourceError (loc, ResourceComputation, e)) (get_raw_backtrace ()))

let empty_usage_map = Hyp_map.empty

(* TODO?
Resources.Computation.compute_resource
Resources.compute = Resources.Computation.compute_resource
when dune supports this.
*)
(* compute_resource = compute_resources_and_merge_usage ~current_usage:(empty_usage_map res) *)
(* FIXME: #odoc why is annotation required on callees? *)
let rec compute_resources ?(expected_res: resource_spec) (res: resource_spec) (t: trm): resource_usage_map option * resource_spec =
  if debug_print_computation_stack then Printf.eprintf "With resources: %s\nComputing %s\n\n" (resources_to_string res) (AstC_to_c.ast_to_string t);
  t.ctx.ctx_resources_before <- res;
  let (let**) (x: 'a option) (f: 'a -> 'b option * 'c option) =
    match x with
    | Some x -> f x
    | None -> None, None
  in
  let usage_map, res =
    let** res in
    try begin match t.desc with
    | Trm_val _ | Trm_var _ -> (Some Var_map.empty, Some res) (* TODO: Manage return values for pointers *)

    | Trm_let_fun (name, ret_type, args, body, contract) ->
      (* TODO: Remove trm_let_fun *)
      compute_resources (Some res) (trm_let Var_immutable (name, typ_auto ()) (trm_fun args (Some ret_type) body ~contract))

    | Trm_fun (args, ret_type, body, contract) ->
      let compute_resources_in_body contract =
        let body_res = Resource_set.bind ~old_res:res ~new_res:contract.pre in
        (* LATER: Merge used pure facts *)
        ignore (compute_resources ~expected_res:contract.post (Some body_res) body);
      in
      begin match contract with
      | FunSpecContract contract ->
        compute_resources_in_body contract;
        let args = List.map (fun (x, _) -> x) args in
        (Some Var_map.empty, Some { res with fun_specs = Var_map.add var_result {args; contract; inverse = None} res.fun_specs })
      | FunSpecReverts reverted_fn ->
        (* LATER: allow non empty arg list for reversible functions, this requires subtitution in the reversed contract *)
        assert (args = []);
        (* TODO: Also register reverse in the fun_contracts entry *)
        let reverted_spec = Var_map.find reverted_fn res.fun_specs in
        assert (reverted_spec.args = []);
        let reverse_contract = revert_fun_contract reverted_spec.contract in
        compute_resources_in_body reverse_contract;
        let args = List.map (fun (x, _) -> x) args in
        let fun_specs =
          res.fun_specs |>
          Var_map.add reverted_fn { reverted_spec with inverse = Some var_result } |>
          Var_map.add var_result { args; contract = reverse_contract; inverse = Some reverted_fn }
        in
        (Some Var_map.empty, Some { res with fun_specs })
      | FunSpecUnknown -> (Some Var_map.empty, Some res)
      end

    | Trm_seq instrs ->
      let instrs = Mlist.to_list instrs in
      let usage_map, res = List.fold_left (fun (usage_map, res) inst ->
          compute_resources_and_merge_usage res usage_map inst)
          (Some Var_map.empty, Some res) instrs
      in

      let res = Option.map (fun res ->
        (* Free the cells allocated with stack new *)
        let extract_let_mut ti =
          match trm_let_inv ti with
          | Some (_, x, _, t) ->
            begin match new_operation_inv t with
            | Some _ -> [x]
            | None -> []
            end
          | None -> []
        in
        let to_free = List.concat_map extract_let_mut instrs in
        (*Printf.eprintf "Trying to free %s from %s\n\n" (String.concat ", " to_free) (resources_to_string (Some res));*)
        let res_to_free = Resource_set.make ~linear:(List.map (fun x -> (new_anon_hyp (), formula_uninit (formula_cell x))) to_free) () in
        let _, _, linear = extract_resources ~split_frac:false res res_to_free in
        { res with linear }) res
      in
      usage_map, res

    | Trm_let (_, (var, typ), body) ->
      let usage_map, res_after = compute_resources (Some res) body in
      let res_after = Option.map (fun res_after -> match formula_of_trm body with
        | Some f_body ->
          let f_body = trm_subst res_after.aliases f_body in
          { res_after with aliases = Var_map.add var f_body res_after.aliases }
        | _ -> res_after)
        res_after
      in
      usage_map, Option.map (fun res_after -> Resource_set.rename_var var_result var res_after) res_after

    | Trm_apps (fn, effective_args, ghost_args) ->
      begin match find_fun_spec fn res.fun_specs with
      | spec ->
        (* The arguments of the function call cannot have write effects, and cannot be referred to in the function contract unless they are formula-convertible (cf. formula_of_term). *)
        (* TODO: Cast into readonly: better error message when a resource exists in RO only but asking for RW *)
        let read_only_res = Resource_set.read_only res in
        let compute_and_check_resources_in_arg usage_map arg =
          (* Give resources as read only and check that they are still there after the argument evaluation *)
          (* LATER: Collect pure facts of arguments:
              f(3+i), f(g(a)) where g returns a + a, f(g(a)) where g ensures res mod a = 0 *)
          let usage_map, post_arg_res = compute_resources_and_merge_usage (Some read_only_res) usage_map arg in
          begin match post_arg_res with
          | Some post_arg_res ->
            begin try ignore (assert_resource_impl post_arg_res (Resource_set.make ~linear:read_only_res.linear ()))
            with e -> raise (ImpureFunctionArgument e)
            end
          | None -> ()
          end;
          usage_map
        in
        (* Check the function itself as if it is an argument of the call (useful for closures) *)
        let usage_map = compute_and_check_resources_in_arg (Some Var_map.empty) fn in

        let contract_fv = fun_contract_free_vars spec.contract in
        let subst_ctx, usage_map = try
          List.fold_left2 (fun (subst_map, usage_map) contract_arg effective_arg ->
            let usage_map = compute_and_check_resources_in_arg usage_map effective_arg in

            (* For all effective arguments that will appear in the instantiated contract,
               check that they have a formula interpretation *)
            if Var_set.mem contract_arg contract_fv then begin
              let arg_formula = match formula_of_trm effective_arg with
                | Some formula -> formula
                | None -> trm_fail effective_arg (Printf.sprintf "Could not make a formula out of term '%s', required because of instantiation of %s contract" (AstC_to_c.ast_to_string effective_arg) (AstC_to_c.ast_to_string fn))
              in
              Var_map.add contract_arg arg_formula subst_map, usage_map
            end else
              subst_map, usage_map
          ) (Var_map.empty, usage_map) spec.args effective_args;
        with Invalid_argument _ ->
          failwith (Printf.sprintf "Mismatching number of arguments for %s" (AstC_to_c.ast_to_string fn))
        in

        let ghost_args_vars = ref Var_set.empty in
        let subst_ctx = List.fold_left (fun subst_ctx (ghost_var, ghost_inst) ->
          if Var_set.mem ghost_var !ghost_args_vars then (failwith (sprintf "Ghost argument %s given twice for function %s" (var_to_string ghost_var) (AstC_to_c.ast_to_string fn)));
          ghost_args_vars := Var_set.add ghost_var !ghost_args_vars;
          Var_map.add ghost_var ghost_inst subst_ctx) subst_ctx ghost_args
        in
        let contract = { spec.contract with pre = { spec.contract.pre with pure = List.filter (fun (ghost_var, _) ->
          let manually_given = Var_set.mem ghost_var !ghost_args_vars in
          ghost_args_vars := Var_set.remove ghost_var !ghost_args_vars;
          not manually_given) spec.contract.pre.pure } }
        in
        assert (Var_set.is_empty !ghost_args_vars);

        let call_usage_map, res_after = compute_contract_invoc contract ~subst_ctx res t in
        let usage_map = Option.map (fun usage_map -> update_usage_map ~current_usage:usage_map ~extra_usage:call_usage_map) usage_map in

        usage_map, Some res_after

      | exception Spec_not_found fn when var_eq fn Resource_primitives.__cast ->
        (* TK: we treat cast as identity function. *)
        (* FIXME: this breaks invariant that function arguments are pure/reproducible. *)
        begin match effective_args with
        | [arg] -> compute_resources (Some res) arg
        | _ -> failwith "expected 1 argument for cast"
        end

      | exception Spec_not_found fn when var_eq fn ghost_begin ->
        let ghost_call = Pattern.pattern_match effective_args [
          Pattern.(trm_apps (trm_apps2 (trm_var (var_eq with_reverse)) !__ !__) nil !__ ^:: nil) (fun ghost_fn ghost_fn_rev ghost_args ->
            let spec = find_fun_spec ghost_fn res.fun_specs in
            let reverse_contract = revert_fun_contract spec.contract in
            begin match trm_fun_inv ghost_fn_rev with
            | Some ([], ret_typ, body, _) ->
              ignore (compute_resources (Some res) (trm_fun [] ret_typ body ~contract:(FunSpecContract reverse_contract)))
            | Some _ -> failwith "A ghost reverse function should have no arguments"
            | None ->
              ignore (compute_resources (Some reverse_contract.pre) ~expected_res:reverse_contract.post (trm_apps ghost_fn_rev [] ~ghost_args))
            end;
            (trm_apps ghost_fn [] ~ghost_args)
          );
          Pattern.(!(trm_apps !__ nil __) ^:: nil) (fun ghost_call ghost_fn ->
            let spec = find_fun_spec ghost_fn res.fun_specs in
            begin match spec.inverse with
            | Some _ -> ()
            | None -> failwith (sprintf "%s is not reversible but is used inside __ghost_begin" (var_to_string fn))
            end;
            ghost_call
          );
          Pattern.(!__) (fun _ -> failwith "expected a ghost call inside __ghost_begin")
        ] in
        let usage_map, res = compute_resources (Some res) ghost_call in
        begin match res, ghost_call.ctx.ctx_resources_contract_invoc with
        | Some res, Some invoc ->
          assert (invoc.contract_produced.produced_pure = []);
          let inverse_pre = List.map (fun { produced_hyp; produced_formula } -> (produced_hyp, produced_formula)) invoc.contract_produced.produced_linear in
          let inverse_post = List.map (fun { pre_hyp; used_formula } -> (pre_hyp, used_formula))
            invoc.contract_inst.used_linear
          in
          let inverse_spec = { args = [];
            contract = { pre = Resource_set.make ~linear:inverse_pre (); post = Resource_set.make ~linear:inverse_post () };
            inverse = None }
          in
          usage_map, Some { res with fun_specs = Var_map.add var_result inverse_spec res.fun_specs }
        | _ -> failwith "Ghost call inside __ghost_begin should have generated a contract_invoc"
        end

      | exception Spec_not_found fn when var_eq fn ghost_end ->
        Pattern.pattern_match effective_args [
          Pattern.(!(trm_var !__) ^:: nil) (fun fn fn_var ->
            (* LATER: Maybe check that the variable is indeed introduced by __ghost_begin *)
            let usage_map, res = compute_resources (Some res) (trm_apps fn []) in
            usage_map, Option.map (fun res -> { res with fun_specs = Var_map.remove fn_var res.fun_specs }) res
          );
          Pattern.(!__) (fun _ -> failwith "__ghost_end expects a single variable as argument")
        ]

      | exception Spec_not_found fn when var_eq fn __admitted ->
        None, None
      end

    | Trm_for (range, body, None) ->
      (* If no spec is given, put all the resources in the invariant (best effort) *)
      let expected_res = Resource_set.make ~linear:(List.map (fun (_, f) -> (new_anon_hyp (), f)) res.linear) () in
      let usage_map, _ = compute_resources ~expected_res (Some res) body in
      usage_map, Some (Resource_set.bind ~old_res:res ~new_res:expected_res)

    | Trm_for (range, body, Some contract) ->
      let outer_contract = contract_outside_loop range contract in
      let usage_map, res_after = compute_contract_invoc outer_contract res t in

      let inner_contract = contract_inside_loop range contract in
      ignore (compute_resources ~expected_res:inner_contract.post (Some (Resource_set.bind ~old_res:res ~new_res:inner_contract.pre)) body);

      Some usage_map, Some res_after

    | Trm_typedef _ ->
      Some Var_map.empty, Some res

    | Trm_template (params, t) ->
      compute_resources (Some res) t

    | _ -> trm_fail t ("Resource_core.compute_inplace: not implemented for " ^ AstC_to_c.ast_to_string t)
    end with e -> handle_resource_errors t.loc e
  in

  t.ctx.ctx_resources_usage <- usage_map;
  if debug_print_computation_stack then Printf.eprintf "With resources: %s\nSaving %s\n\n" (resources_to_string res) (AstC_to_c.ast_to_string t);
  t.ctx.ctx_resources_after <- res;
  let res =
    match res, expected_res with
    | Some res, Some expected_res ->
      begin try
        let used_res = assert_resource_impl res expected_res in
        t.ctx.ctx_resources_post_inst <- Some used_res
      with e when !Flags.resource_errors_as_warnings ->
        Printf.eprintf "%s: Resource check warning: %s\n" (loc_to_string t.loc) (Printexc.to_string e);
      | ResourceError _ as e -> raise e
      | e -> raise (ResourceError (t.loc, ResourceCheck, e))
      end;
      Some expected_res
    | _, None -> res
    | None, _ -> expected_res
  in
  usage_map, res

and compute_resources_and_merge_usage ?(expected_res: resource_spec) (res: resource_spec) (current_usage: resource_usage_map option) (t: trm): resource_usage_map option * resource_spec =
  let extra_usage, res = (compute_resources : ?expected_res:resource_set -> resource_spec -> trm -> (resource_usage_map option * resource_spec)) ?expected_res res t in
  try
    let usage_map = update_usage_map_opt ~current_usage ~extra_usage in
    (usage_map, res)
  with e -> handle_resource_errors t.loc e


let rec trm_deep_copy (t: trm) : trm =
  let t = trm_map_with_terminal ~share_if_no_change:false false (fun _ ti -> trm_deep_copy ti) t in
  t.ctx <- unknown_ctx (); (* LATER *)
  t

(* hypothesis: needs var_ids to be calculated *)
let trm_recompute_resources (init_ctx: resource_set) (t: trm): trm =
  (* TODO: should we avoid deep copy by maintaining invariant that there is no sharing?
     makes sense with unique var ids and trm_copy mechanisms.
     Otherwise avoid mutable asts. Could also have unique node ids and maps from ids to metadata. *)
  let t = trm_deep_copy t in
  ignore (compute_resources (Some init_ctx) t);
  t
