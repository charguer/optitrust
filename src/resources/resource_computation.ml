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
    | Prim_new (_, []) -> "__new"
    (* | Prim_new (_, dims) -> "__new_array"  SAME AS MALLOCN but with implicit trm_apps *)
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

  let inst_split_read_only ~(new_frac: var) ~(old_frac: formula) (h: hyp) : t =
    trm_apps (trm_var var_SplitRO) [trm_var new_frac; old_frac; inst_hyp h]

  let inst_split_read_only_inv (f: t): (var * formula * hyp) option =
    Pattern.pattern_match_opt f [
      Pattern.(trm_apps3 (trm_var (var_eq var_SplitRO)) (trm_var !__) !__ (trm_var !__)) (fun frac old_frac hyp -> (frac, old_frac, hyp))
    ]

  let var_ForgetInit = toplevel_var "ForgetInit"

  let inst_forget_init (h: hyp) : t =
    trm_apps (trm_var var_ForgetInit) [inst_hyp h]

  let inst_forget_init_inv (f: t): hyp option =
    Pattern.pattern_match_opt f [
      Pattern.(trm_apps1 (trm_var (var_eq var_ForgetInit)) (trm_var !__)) (fun h -> h)
    ]

  let origin_hyp (f: t): hyp =
    match inst_hyp_inv f with
    | Some h -> h
    | None ->
      match inst_split_read_only_inv f with
      | Some (_, _, h) -> h
      | None ->
        match inst_forget_init_inv f with
        | Some h -> h
        | None -> failwith "Formula_inst.origin_hyp: unrecognized formula inst"
end

(** A type to distinguish between pure and linear resources *)
type resource_kind =
  | Pure
  | Linear

let resource_kind_to_string kind =
  match kind with
  | Pure -> "Pure"
  | Linear -> "Linear"

(** [Resource_not_found (kind, item, res_list)]: exception raised when the resource
   [item] is not found inside the resource list [res_list] *)
exception Resource_not_found of resource_kind * resource_item * resource_item list

let raise_resource_not_found (kind: resource_kind) ((name, formula): resource_item) (evar_ctx: unification_ctx) (inside: resource_item list) =
  let subst_ctx = Var_map.mapi (fun var subst ->
    match subst with Some t -> t | None -> trm_var { var with name = ("?" ^ var.name) }
  ) evar_ctx in
  let formula = trm_subst subst_ctx formula in
  let inside = List.map (fun (x, formula) -> (x, trm_subst subst_ctx formula)) inside in
  raise (Resource_not_found (kind, (name, formula), inside))

let arith_goal_solver ((x, formula): resource_item) (evar_ctx: unification_ctx): unification_ctx option =
  let subst_ctx = Var_map.fold (fun var subst ctx ->
    match subst with
    | Some t -> Var_map.add var t ctx
    | None -> ctx) evar_ctx Var_map.empty
  in
  let formula = trm_subst subst_ctx formula in
  let arith_solved = Pattern.pattern_match formula [
    Pattern.(trm_apps2 (trm_var (var_eq var_in_range)) !__ (formula_range !__ !__ !__)) (fun index start stop step ->
      Arith_core.(check_geq index start && check_lt index stop && check_eq (trm_mod index step) (trm_int 0))
    );
    Pattern.(trm_apps2 (trm_var (var_eq var_is_subrange)) (formula_range !__ !__ !__) (formula_range !__ !__ !__)) (fun sub_start sub_stop sub_step start stop step ->
      Arith_core.(check_geq sub_start start && check_leq sub_stop stop && check_eq (trm_mod sub_step step) (trm_int 0))
    );
    Pattern.(trm_apps2 (trm_var (var_eq formula_assert_eq)) !__ !__) Arith_core.check_eq;
    Pattern.(trm_apps2 (trm_var (var_eq formula_assert_neq)) !__ !__) Arith_core.check_neq;
    Pattern.(trm_apps2 (trm_var (var_eq formula_assert_gt)) !__ !__) Arith_core.check_gt;
    Pattern.(trm_apps2 (trm_var (var_eq formula_assert_geq)) !__ !__) Arith_core.check_geq;
    Pattern.(trm_apps2 (trm_var (var_eq formula_assert_lt)) !__ !__) Arith_core.check_lt;
    Pattern.(trm_apps2 (trm_var (var_eq formula_assert_leq)) !__ !__) Arith_core.check_leq;
    Pattern.__ false
  ] in
  if arith_solved then
    let evar_ctx = Var_map.add x (Some formula_arith_checked) evar_ctx in
    Some evar_ctx
  else
    None


(** [unify_pure (x, formula) res evar_ctx] unifies the given [formula] with one of the resources in [res].

   If none of the resources match, the optional goal solver is tried on the formula to try to discharge it.

   Also add a binding from [x] to the found resource in [evar_ctx].
   If it fails raise a {!Resource_not_found} exception. *)
let unify_pure ((x, formula): resource_item) (res: pure_resource_set) ?(goal_solver = arith_goal_solver) (evar_ctx: unification_ctx): unification_ctx =
  (* Add flag to disallow pure instantiation *)
  let find_formula formula (hyp_candidate, formula_candidate) =
    Option.map (fun evar_ctx -> Var_map.add x (Some (trm_var hyp_candidate)) evar_ctx)
      (unify_trm formula_candidate formula evar_ctx)
  in
  match List.find_map (find_formula formula) res with
  | Some evar_ctx -> evar_ctx
  | None ->
    match goal_solver (x, formula) evar_ctx with
    | Some evar_ctx -> evar_ctx
    | None -> raise_resource_not_found Pure (x, formula) evar_ctx res


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

  Raises {!Not_found} if the [formula] cannot be consumed.
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
        { pre_hyp ; inst_by = Formula_inst.inst_split_read_only ~new_frac ~old_frac:cur_frac h; used_formula = formula_read_only ~frac:(trm_var new_frac) formula_candidate },
        Some (h, formula_read_only ~frac:(trm_sub cur_frac (trm_var new_frac)) formula_candidate), evar_ctx)
    ) res evar_ctx
  in

  let formula, evar_ctx = unfold_if_resolved_evar formula evar_ctx in
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
let subtract_linear_resource_set ?(split_frac: bool = false) ?(evar_ctx: unification_ctx = Var_map.empty) (res_from: linear_resource_set) (res_removed: linear_resource_set)
  : used_resource_item list * linear_resource_set * unification_ctx =
  List.fold_left (fun (used_list, res_from, evar_ctx) res_item ->
    try
      let used, res_from, evar_ctx = subtract_linear_resource_item ~split_frac res_item res_from evar_ctx in
      (used :: used_list, res_from, evar_ctx)
    with Not_found ->
      raise_resource_not_found Linear res_item evar_ctx res_from
  ) ([], res_from, evar_ctx) res_removed

(** [partial_extract_linear_resource_set] computes the intersection between two linear resource sets
    If [evar_ctx] is provided, this is done up to evar unification.
  Returns [(used, res_left', res_right', evar_ctx')] where:
  - [used] are the consumed resource items
  - [res_left'] is what remains from [res_left]
  - [res_right'] is what remains from [res_right]
  - [evar_ctx'] is the evar context updated with new unification choices

  FIXME: not symmetrical left/right doc (uninit)
*)
let partial_extract_linear_resource_set ?(evar_ctx: unification_ctx = Var_map.empty) (res_left: linear_resource_set) (res_right: linear_resource_set)
  : used_resource_item list * linear_resource_set * linear_resource_set * unification_ctx =
  List.fold_left (fun (used_list, res_left, unmatched_right, evar_ctx) res_item ->
    try
      let used, res_left, evar_ctx = subtract_linear_resource_item ~split_frac:false res_item res_left evar_ctx in
      (used :: used_list, res_left, unmatched_right, evar_ctx)
    with Not_found ->
      (used_list, res_left, res_item :: unmatched_right, evar_ctx)
  ) ([], res_left, [], evar_ctx) res_right


(** [eliminate_dominated_evars res] eliminates dominated evars from [res].
  Returns the the remaining pure resources and the dominated evars.

  An evar is dominated if its value will be implied by the instantation of other resources
  (i.e. it appears in the formula of another resource). *)
let eliminate_dominated_evars (res: resource_set): pure_resource_set * var list =
  (* LATER: Un tri topologique serait un peu plus robuste *)
  let used_vars = Resource_set.used_vars res in
  List.partition_map (function
    | h, _ when Var_set.mem h used_vars -> Either.Right h
    | res -> Either.Left res) res.pure

(** [update_usage hyp current_usage extra_usage] returns the usage resulting from using
    [current_usage] before using [extra_usage]. *)
let update_usage (hyp: hyp) (current_usage: resource_usage option) (extra_usage: resource_usage option): resource_usage option =
  match current_usage, extra_usage with
  | None, u -> u
  | u, None -> u
  | Some Required, Some Required -> Some Required
  | Some Ensured, Some Required -> Some Ensured
  | Some Required, Some Ensured -> failwith (sprintf "Ensured resource %s share id with another one" (var_to_string hyp))
  | Some (Required | Ensured), _ | _, Some (Required | Ensured) ->
    failwith (sprintf "Resource %s is used both as a pure and as a linear resource" (var_to_string hyp))
  | Some Produced, Some (ConsumedFull | ConsumedUninit) -> None
  | Some Produced, Some (SplittedFrac | JoinedFrac) -> Some Produced
  | _, Some Produced -> failwith (sprintf "Produced resource %s share id with another one" (var_to_string hyp))
  | Some (ConsumedFull | ConsumedUninit), _ -> failwith (sprintf "Consumed resource %s share id with another one" (var_to_string hyp))
  | Some (SplittedFrac | JoinedFrac), Some (ConsumedFull | ConsumedUninit) -> Some ConsumedFull
  | Some JoinedFrac, Some JoinedFrac -> Some JoinedFrac
  | Some (SplittedFrac | JoinedFrac), Some (SplittedFrac | JoinedFrac) -> Some SplittedFrac

(** [update_usage_map ~current_usage ~extra_usage] returns the usage map resulting from using
    [current_usage] before using [extra_usage]. *)
let update_usage_map ~(current_usage: resource_usage_map) ~(extra_usage: resource_usage_map): resource_usage_map =
  Hyp_map.merge update_usage current_usage extra_usage

let update_usage_map_opt ~(current_usage: resource_usage_map option) ~(extra_usage: resource_usage_map option): resource_usage_map option =
  let open Xoption.OptionMonad in
  let* current_usage in
  let* extra_usage in
  Some (update_usage_map ~current_usage ~extra_usage)

(** [add_usage hyp extra_usage usage_map] adds the [extra_usage] of hypothesis [hyp] to the [usage_map]. *)
let add_usage (hyp: hyp) (extra_usage: resource_usage) (usage_map: resource_usage_map): resource_usage_map =
  let current_usage = Hyp_map.find_opt hyp usage_map in
  match update_usage hyp current_usage (Some extra_usage) with
  | None -> Hyp_map.remove hyp usage_map
  | Some new_usage -> Hyp_map.add hyp new_usage usage_map

(** [used_set_to_usage_map res_used] converts the used resource set [res_used] into the corresponding usage map. *)
let used_set_to_usage_map (res_used: used_resource_set) : resource_usage_map =
  let pure_usage = List.fold_left (fun usage_map { inst_by } ->
      let used_fv = trm_free_vars inst_by in
      Var_set.fold (fun x -> Hyp_map.add x Required) used_fv usage_map)
    Hyp_map.empty res_used.used_pure
  in
  List.fold_left (fun usage_map { inst_by; used_formula } ->
    match Formula_inst.inst_split_read_only_inv inst_by with
    | Some (_, _, orig_hyp) -> Hyp_map.add orig_hyp SplittedFrac usage_map
    | None ->
      let hyp_usage = if formula_uninit_inv used_formula = None then ConsumedFull else ConsumedUninit in
      match Formula_inst.inst_hyp_inv inst_by with
      | Some hyp -> Hyp_map.add hyp hyp_usage usage_map
      | None ->
        match Formula_inst.inst_forget_init_inv inst_by with
        | Some hyp -> Hyp_map.add hyp hyp_usage usage_map
        | None -> failwith "Weird resource used"
  ) pure_usage res_used.used_linear

let new_efracs_from_used_set (res_used: used_resource_set): (hyp * formula) list =
  List.filter_map (fun { inst_by } ->
    match Formula_inst.inst_split_read_only_inv inst_by with
    | Some (efrac, bigger_frac, _) -> Some (efrac, bigger_frac)
    | None -> None
  ) res_used.used_linear

exception Spec_not_found of var
exception Anonymous_function_without_spec
exception NotConsumedResources of linear_resource_set
exception ImpureFunctionArgument of exn
exception FractionConstraintUnsatisfied of formula * formula

type frac_quotient = { base: formula; num: int; den: int }

let rec frac_to_quotient (frac: formula) =
  Pattern.pattern_match frac [
    Pattern.(trm_sub !__ !__) (fun base_frac removed_frac ->
      let base_quot = frac_to_quotient base_frac in
      let removed_quot = frac_to_quotient removed_frac in
      if not (are_same_trm base_quot.base removed_quot.base) then raise Pattern.Next;
      let num =
        if base_quot.den = 1 && base_quot.num = 1 then
          removed_quot.den - removed_quot.num
        else if base_quot.den = removed_quot.den then
          base_quot.num - removed_quot.num
        else raise Pattern.Next
      in
      { removed_quot with num }
    );
    Pattern.(trm_div !__ (trm_int !__)) (fun base_frac den ->
      { base = base_frac; num = 1; den = den }
    );
    Pattern.(!__) (fun _ ->
      { base = frac; num = 1; den = 1 }
    )
  ]

let check_frac_le subst_ctx (efrac, bigger_frac) =
  let efrac = Var_map.find efrac subst_ctx in
  let bigger_frac = trm_subst subst_ctx bigger_frac in
  let efrac_quot = frac_to_quotient efrac in
  let bigger_quot = frac_to_quotient bigger_frac in
  if not (are_same_trm efrac_quot.base bigger_quot.base) then raise (FractionConstraintUnsatisfied (efrac, bigger_frac));
  if not (bigger_quot.den = 1 && bigger_quot.num = 1 && efrac_quot.num <= efrac_quot.den) then begin
    if not (efrac_quot.den = bigger_quot.den) then raise (FractionConstraintUnsatisfied (efrac, bigger_frac));
    if not (efrac_quot.num <= bigger_quot.num) then raise (FractionConstraintUnsatisfied (efrac, bigger_frac));
  end


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
let extract_resources ~(split_frac: bool) (res_from: resource_set) ?(subst_ctx: tmap = Var_map.empty) ?(specialize_efracs: bool = false) (res_to: resource_set) : tmap * used_resource_set * linear_resource_set =
  let not_dominated_pure_res_to, dominated_evars = eliminate_dominated_evars res_to in
  let evar_ctx = Var_map.map (fun x -> Some (trm_subst res_from.aliases x)) subst_ctx in
  let evar_ctx = List.fold_left (fun evar_ctx x -> Var_map.add x None evar_ctx) evar_ctx dominated_evars in

  let res_from = Resource_set.subst_all_aliases res_from in
  assert (Var_map.is_empty res_to.aliases);
  assert (res_to.efracs = []);

  let efracs = if specialize_efracs then res_from.efracs else [] in
  let evar_ctx = List.fold_left (fun evar_ctx (efrac, _) -> Var_map.add efrac None evar_ctx) evar_ctx efracs in

  let used_linear, leftover_linear, evar_ctx = subtract_linear_resource_set ~split_frac ~evar_ctx res_from.linear res_to.linear in
  let evar_ctx = List.fold_left (fun evar_ctx res_item ->
      unify_pure res_item res_from.pure evar_ctx) evar_ctx not_dominated_pure_res_to
  in

  (* All dominated evars should have been instantiated at this point.
     There is a bug if it's not the case. *)
  let subst_ctx = Var_map.map (function
      | Some t -> t
      | None ->
        let inst_failed_evars = Var_map.filter (fun _ -> Option.is_none) evar_ctx in
        let inst_failed_evars_str = String.concat ", " (List.map (fun (evar, _) -> evar.name) (Var_map.bindings inst_failed_evars)) in
        failwith ("failed to instantiate evars " ^ inst_failed_evars_str)) evar_ctx
  in

  (* Check that efrac constaints are satisfied *)
  (* FIXME: It is probably unsound to always check f <= g goals, some inequalities should be strict. *)
  List.iter (check_frac_le subst_ctx) efracs;

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

  (* Filter out all leftovers with a null fraction *)
  let leftover_linear = List.filter (fun (h, formula) ->
    match formula_read_only_inv formula with
    | Some { frac } ->
      let frac_quotient = frac_to_quotient (trm_subst subst_ctx frac) in
      frac_quotient.num <> 0
    | None -> true
  ) leftover_linear in

  (subst_ctx, { used_pure; used_linear }, leftover_linear)

(* FIXME: resource set intuition breaks down, should we talk about resource predicates? *)
(** [assert_resource_impl res_from res_to] checks that [res_from] ==> [res_to]. *)
let assert_resource_impl (res_from: resource_set) (res_to: resource_set) : used_resource_set =
  let _, used_res, leftovers = extract_resources ~split_frac:false ~specialize_efracs:true res_from res_to in
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

(** Forgets information from [produced_resource_set]. *)
let produced_resources_to_resource_set (res_produced: produced_resource_set): resource_set =
  let forget_origin =
    List.map (fun { produced_hyp; produced_formula } -> (produced_hyp, produced_formula))
  in
  let pure = forget_origin res_produced.produced_pure in
  let linear = forget_origin res_produced.produced_linear in
  Resource_set.make ~pure ~linear ()

(** Internal type to represent RO formula frac wands. *)
type frac_wand = formula * formula list
type frac_wand_list = (hyp * frac_wand) list
type frac_simplification_steps = (hyp * hyp) list

let rec formula_to_frac_wand (frac: formula) : frac_wand =
  match trm_binop_inv Binop_sub frac with
  | Some (sub_frac, carved_frac) ->
    let (base_frac, carved_fracs) = formula_to_frac_wand sub_frac in
    (base_frac, carved_frac :: carved_fracs)
  | None -> (frac, [])

let frac_wand_to_formula ((base_frac, carved_fracs): frac_wand): formula =
  List.fold_right (fun carved_frac frac -> trm_sub frac carved_frac) carved_fracs base_frac

(** [simplify_frac_wands frac_wands] try to simplify [frac_wands] on the same resource between themselves.

    For each frac_wand (g - h1 - .. - hn),
    - for each [hi] we try to join the current fraction with another one that has [hi] in the head position
    - then, once we are stuck, we look if [g] occurs in one of the minuses (carved frac) of the other wands
*)
let rec simplify_frac_wands (frac_wands: (hyp * frac_wand) list): frac_simplification_steps * frac_wand_list =
  let rec try_pop_base_frac (base_frac: formula) (frac_wands: frac_wand_list) =
    let open Xoption.OptionMonad in
    match frac_wands with
    | [] -> None
    | (hyp, (wand_base_frac, carved_fracs)) :: frac_wands when are_same_trm base_frac wand_base_frac ->
      Some (frac_wands, hyp, carved_fracs)
    | non_matching_wand :: frac_wands ->
      let* frac_wands, hyp, carved_fracs = try_pop_base_frac base_frac frac_wands in
      Some (non_matching_wand :: frac_wands, hyp, carved_fracs)
  in

  let rec try_simplify_carved_fracs (carved_fracs: formula list) (frac_wands: frac_wand_list) =
    match carved_fracs with
    | [] -> [], frac_wands, []
    | carved_frac :: carved_fracs ->
      match try_pop_base_frac carved_frac frac_wands with
      | None ->
        let remaining_carved_fracs, frac_wands, consumed_hyps = try_simplify_carved_fracs carved_fracs frac_wands in
        carved_frac :: remaining_carved_fracs, frac_wands, consumed_hyps
      | Some (frac_wands, hyp, new_carved_fracs) ->
        let remaining_carved_fracs, frac_wands, consumed_hyps = try_simplify_carved_fracs (new_carved_fracs @ carved_fracs) frac_wands in
        remaining_carved_fracs, frac_wands, hyp :: consumed_hyps
  in

  let rec try_push_into_carved_frac base_frac new_carved_fracs frac_wands =
    let open Xoption.OptionMonad in
    match frac_wands with
    | [] -> None
    | (hyp, (wand_base_frac, wand_carved_fracs)) as cur_wand :: frac_wands ->
      let rec try_push_into carved_fracs =
        match carved_fracs with
        | [] -> None
        | carved_frac :: other_carved_fracs when are_same_trm base_frac carved_frac ->
          Some (new_carved_fracs @ other_carved_fracs)
        | non_matching_carved_frac :: carved_fracs ->
          let* carved_fracs = try_push_into carved_fracs in
          Some (non_matching_carved_frac :: carved_fracs)
      in
      match try_push_into wand_carved_fracs with
      | Some carved_fracs -> Some (hyp, (hyp, (wand_base_frac, carved_fracs)) :: frac_wands)
      | None ->
        let* hyp, frac_wands = try_push_into_carved_frac base_frac new_carved_fracs frac_wands in
        Some (hyp, cur_wand :: frac_wands)
  in

  match frac_wands with
  | [] -> [], []
  | (hyp, (base_frac, carved_fracs)) :: frac_wands ->
    let remaining_carved_fracs, frac_wands, consumed_hyps = try_simplify_carved_fracs carved_fracs frac_wands in
    let simpl_steps = List.map (fun ch -> hyp, ch) consumed_hyps in
    match try_push_into_carved_frac base_frac remaining_carved_fracs frac_wands with
    | None ->
      let next_simpl_steps, frac_wands = simplify_frac_wands frac_wands in
      (simpl_steps @ next_simpl_steps, (hyp, (base_frac, remaining_carved_fracs)) :: frac_wands)
    | Some (target_hyp, frac_wands) ->
      let next_simpl_steps, frac_wands = simplify_frac_wands frac_wands in
      (simpl_steps @ (target_hyp, hyp) :: next_simpl_steps, frac_wands)

  (* LATER: ideas for a more efficient algorithm:
    we can use a table to cache the items to avoid repeated comparisons and traversals

    input items: (g1, [h11; ..; h1n]), (g2, [h21; ..; h2n])
    working map: [gi->[hi1; ..; hin]]
    auxiliary map: [hij->gi]
    result set: []
    while working map not empty
      we pop one item from working map, e.g. (g1, [h11; ..; h1n])
      remaining = []
      put the h1k in a queue
      while queue not empty,
        h1k next one in the queue
        remove auxiliary map form auxiliary map
        if working.mem h1k
          let [hj1...hjn] = working.pop h1k
          push the [hj1...hjn] into the queue
        else
          remaining.push h1k

      // look for g1 in others
      let gi = auxiliary.find_opt(his)
      | None -> result.push(g1k, remaining)
      | Some (his) ->
            working.push(gi, (List.remove g1 his) ++ remaining)
            List.iter his (fun ha -> auxiliary.push[ha -> gi]
  *)


(** [simplify_read_only_resources res] tries to simplify all read only resources from [res]
    joining all the fractions such that there is no pair of resources of the form
    [RO(h_i - f_1 - ... - f_n, H), RO(g - h_0 - ... - h_i - ... - h_n, H)]
 *)
let simplify_read_only_resources (res: linear_resource_set): (linear_resource_set * frac_simplification_steps) =
  let rec find_bucket_and_add formula hyp frac buckets =
    match buckets with
    | [] -> [formula, [hyp, formula_to_frac_wand frac]]
    | (other_formula, fracs) :: other_buckets when are_same_trm formula other_formula ->
      (other_formula, (hyp, formula_to_frac_wand frac) :: fracs) :: other_buckets
    | non_matching_bucket :: other_buckets ->
      non_matching_bucket :: (find_bucket_and_add formula hyp frac other_buckets)
  in
  let ro_buckets = ref [] in
  let non_ro_res = List.filter (fun (hyp, formula) ->
    match formula_read_only_inv formula with
    | None -> true
    | Some { formula; frac } ->
      ro_buckets := find_bucket_and_add formula hyp frac !ro_buckets;
      false
    ) res in
  let simpl_steps = ref [] in
  let ro_buckets = List.map (fun (formula, fracs) ->
      let new_simpl_steps, frac_wands = simplify_frac_wands fracs in
      simpl_steps := new_simpl_steps @ !simpl_steps;
      (formula, frac_wands)
    ) !ro_buckets
  in
  (* Here use a fold_right to maintain the initial ordering between RO resources.
     This is required for ghost_pair_chaining to pass, and maintaining the initial order
     is not a bad idea anyway.
     Note however that this function pulls RO ressources in the initial context in front. *)
  let res = List.fold_right (fun (formula, fracs) res ->
    List.fold_left (fun res (hyp, frac_wand) ->
      let frac = frac_wand_to_formula frac_wand in
      match frac.desc with
      | Trm_val Val_lit Lit_int 1 -> (hyp, formula) :: res
      | _ -> (hyp, formula_read_only ~frac formula) :: res
      ) res fracs
  ) ro_buckets non_ro_res in
  (res, !simpl_steps)

(**
  [resource_merge_after_frame res_after frame] returns [res_after] * [frame] with simplifications.
  Cancels magic wands in [frame] with linear resources in [res_after] and
  returns the produced resource_set.

  Also returns a usage map that corresponds to the new usages from the production of resources in [res_after]
  and the resources merges.

  More precisely, the returned resource set must statifies the following invariants:
  [res_after] * [frame] ==> res
  there is no pair in res of the form RO(h_i - f_1 - ... - f_n, H), RO(g - h_0 - ... - h_i - ... - h_n, H)

  Ex: res_after.linear = RO('a, t)  and  frame = RO('b - 'a, t)
  gives res.linear = RO('b, t)
 *)
let resource_merge_after_frame (res_after: produced_resource_set) (frame: linear_resource_set) : resource_set * resource_usage_map * (hyp * hyp) list =
  let res_after = produced_resources_to_resource_set res_after in
  let used_set = List.fold_left (fun used_set (h, _) -> Hyp_map.add h Ensured used_set) Hyp_map.empty res_after.pure in
  let used_set = List.fold_left (fun used_set (h, _) -> Hyp_map.add h Produced used_set) used_set res_after.linear in

  let linear, ro_simpl_steps = simplify_read_only_resources (res_after.linear @ frame) in
  let used_set = List.fold_left (fun used_set (joined_into, taken_from) ->
    let used_set = add_usage joined_into JoinedFrac used_set in
    let used_set = add_usage taken_from ConsumedFull used_set in
    used_set) used_set ro_simpl_steps in

  { res_after with linear }, used_set, ro_simpl_steps

(** <private> cf. {!find_fun_spec} *)
let trm_fun_var_inv (t:trm): var option =
  try
    match trm_prim_inv t with
    | Some p -> Some (Resource_primitives.to_var p)
    | None -> trm_var_inv t
  with Resource_primitives.Unknown ->
    let trm_internal (msg : string) (t : trm) : string =
      Printf.sprintf "%s: %s\n" msg (Ast_to_text.ast_to_string t) in
    trm_fail t (trm_internal "unimplemented trm_fun_var_inv construction" t)

(** <private> cf. {!compute_resources} *)
let find_fun_spec (t: trm) (fun_specs: fun_spec_resource varmap): fun_spec_resource =
  (** spec is either in env, or attached to term *)
  (* LATER: replace with a query on _Res inside fun_specs, but this requires a management of value aliases *)
  match trm_fun_var_inv t with
  | Some fn ->
    Xoption.unsome_or_else (Var_map.find_opt fn fun_specs) (fun () -> raise (Spec_not_found fn))
  | None ->
    begin match trm_inv ~error:"expected anonymous or named function" trm_fun_inv t with
    | (args, _, _, FunSpecContract contract) ->
      { args = List.map fst args; contract; inverse = None }
    | _ -> raise Anonymous_function_without_spec
    end

let named_formula_to_string nf : string =
  let style = Ast_fromto_AstC.style_of_custom_style (Style.default_custom_style ()) in
  Ast_fromto_AstC.named_formula_to_string style nf

(* FIXME: move printing out of this file. *)
let resource_list_to_string res_list : string =
  String.concat "\n" (List.map named_formula_to_string res_list)

let resource_set_to_string res : string =
  let style = Ast_fromto_AstC.style_of_custom_style (Style.default_custom_style ()) in
  let spure = resource_list_to_string res.pure in
  let sefracs = if res.efracs = [] then ""
    else String.concat "\n" ("" :: List.map (fun efrac -> Ast_fromto_AstC.efrac_to_string style efrac) res.efracs)
  in
  let slin = resource_list_to_string res.linear in
  Printf.sprintf "pure:\n%s%s\n\nlinear:\n%s\n" spure sefracs slin

let resource_set_opt_to_string res : string =
  Xoption.map_or resource_set_to_string "UnspecifiedRes" res

type resource_error_phase = ResourceComputation | PostconditionCheck

let resource_error_phase_to_string (phase: resource_error_phase) : string =
  match phase with
  | ResourceComputation -> "Resource computation"
  | PostconditionCheck -> "Postcondition check"

(** Exception used by [handle_resource_errors] to interrupt typing
    after a first error is discovered. *)
exception StoppedOnFirstError

(** List of errors accumulated during the current call to
    [trm_recompute_resources]. <private> *)
let global_errors : (resource_error_phase * exn) list ref = ref []

(** [ResourceError (t, e)] describes the fact that errors occurred
    during of typing, and [t] describes the partially typed
    term, and [e] describe the first error occurring at phase [p].
    Certain nodes [ti] in [t] may have their field [ti.errors]
    storing the error messages. *)
exception ResourceError of trm * resource_error_phase * exn

let _ = Printexc.register_printer (function
  | Resource_not_found (kind, item, context) ->
    Some (Printf.sprintf "%s resource not found:\n%s\nIn context:\n%s" (resource_kind_to_string kind) (named_formula_to_string item) (resource_list_to_string context))
  | Spec_not_found fn ->
    Some (Printf.sprintf "No specification for function %s" (var_to_string fn))
  | NotConsumedResources res ->
    Some (Printf.sprintf "Resources not consumed after the end of the block:\n%s" (resource_list_to_string res))
  | ImpureFunctionArgument err ->
    Some (Printf.sprintf "Function argument subexpression resource preservation check failed: %s" (Printexc.to_string err))
  | ResourceError (_t, phase, err) ->
    Some (Printf.sprintf "%s error: %s" (resource_error_phase_to_string phase) (Printexc.to_string err));
  | _ -> None)


(** Knowing that term [t] has contract [contract], and that [res] is available before [t],
  [compute_contract_invoc contract ?subst_ctx res t] returns the resources used by [t],
  and the resources available after [t].

  If provided, [subst_ctx] is a substitution applied to [contract].
  Sets [t.ctx.ctx_resources_contract_invoc].
    *)
let compute_contract_invoc (contract: fun_contract) ?(subst_ctx: tmap = Var_map.empty) (res: resource_set) (t: trm): resource_usage_map * resource_set =
  let subst_ctx, res_used, res_frame = extract_resources ~split_frac:true ~subst_ctx res contract.pre in

  let res_produced = compute_produced_resources subst_ctx contract.post in
  let usage = used_set_to_usage_map res_used in

  let new_res, usage_after, ro_simpl_steps = resource_merge_after_frame res_produced res_frame in
  let new_res = { new_res with efracs = new_efracs_from_used_set res_used } in

  t.ctx.ctx_resources_contract_invoc <- Some {
    contract_frame = res_frame;
    contract_inst = res_used;
    contract_produced = res_produced;
    contract_joined_resources = ro_simpl_steps };

  let total_usage = update_usage_map ~current_usage:usage ~extra_usage:usage_after in
  total_usage, Resource_set.(remove_unused_efracs (bind ~keep_efracs:true ~old_res:res ~new_res))

(** <private> *)
let debug_print_computation_stack = false

(** [handle_resource_errors t phase exn] hooks [exn] as error on the term [t],
    and save the fact that an exception [exn] was triggered. *)
let handle_resource_errors (t: trm) (phase:resource_error_phase) (exn: exn) =
  (* Save the error in the term where it occurred, or the referent (transitively) if any *)
  let error_str = sprintf "%s error: %s" (resource_error_phase_to_string phase) (Printexc.to_string exn) in
  let tref = trm_find_referent t in
  if !Flags.debug_errors_msg_embedded_in_ast then begin
    if tref != t
      then Printf.eprintf "GRABBING REFERENT FOR TERM:----\n%s\n-----\n" (AstC_to_c.ast_to_string t);
    Printf.eprintf "SAVING ERROR IN TERM:----\n%s\n-----\n%s-----\n" (AstC_to_c.ast_to_string tref) (Ast_to_text.ast_to_string tref);
  end;
  tref.errors <- error_str :: tref.errors;
  (*Printf.eprintf "ADDERROR %s\n  %s\n" error_str (AstC_to_c.ast_to_string t);*)
  (* Accumulate the error *)
  global_errors := (phase, exn) :: !global_errors;
  (* Interrupt if stop on first error *)
  if !Flags.stop_on_first_resource_error then Printexc.(raise_with_backtrace StoppedOnFirstError (get_raw_backtrace ()));
  (* Return empty resources maps as best effort to continue *)
  None, None


let empty_usage_map = Hyp_map.empty

(* TODO?
Resources.Computation.compute_resource
Resources.compute = Resources.Computation.compute_resource
when dune supports this.
*)
(* compute_resource = compute_resources_and_merge_usage ~current_usage:(empty_usage_map res) *)
(* FIXME: #odoc why is annotation required on callees? *)
(** [compute_resources ?expected_res res t] computes resources within [t], knowing that [res]
    resources are available before [t].
    Returns [(usage, res')].
    If successful, [usage] contains the resources used by [t] and [res'] the resources available
    after [t].
    If unsuccessful [usage = None] and [res' = expected_res].

    If provided, checks that [res' ==> expected_res].

    Sets [t.ctx.ctx_resources_*] fields in depth.
    *)
let rec compute_resources
  ?(expected_res: resource_set option)
  (res: resource_set option)
  (t: trm) : resource_usage_map option * resource_set option =
  if debug_print_computation_stack then Printf.eprintf "=====\nWith resources: %s\nComputing %s\n\n" (resource_set_opt_to_string res) (AstC_to_c.ast_to_string t);
  (* Define the referent for hooking type errors on existing terms
     when errors are triggered on terms that are generated on-the-fly. *)
  let referent : trm_annot =
    Trm.(trm_annot_set_referent trm_annot_default t) in
  t.ctx.ctx_resources_before <- res;
  let (let**) (x: 'a option) (f: 'a -> 'b option * 'c option) =
    match x with
    | Some x -> f x
    | None -> None, None
  in
  let usage_map, res =
    let** res in
    try begin match t.desc with
    (* new array is typed as MALLOCN with correct dims *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_new (ty, dims))) }, _, []) when dims <> [] ->
      compute_resources (Some res) (Matrix_core.alloc_with_ty ~annot:referent ~annot_call:referent dims ty)

    (* Values and variables are pure. *)
    | Trm_val _ | Trm_var _ -> (Some Var_map.empty, Some res) (* TODO: Manage return values for pointers *)

    (* [let_fun f ... = ... types like [let f = fun ... -> ...] *)
    | Trm_let_fun (name, ret_type, args, body, contract) ->
      (* TODO: Remove trm_let_fun *)
      compute_resources (Some res) (trm_let ~annot:referent Var_immutable (name, typ_auto ()) (trm_fun ~annot:referent args (Some ret_type) body ~contract))

    (* Defining a function is pure by itself, we check that the body satisfies the contract.
       If possible, we register a new function specification on [var_result], as well as potential inverse function metadata. *)
    | Trm_fun (args, ret_type, body, contract) ->
      let compute_resources_in_body contract =
        let body_res = Resource_set.bind ~keep_efracs:false ~old_res:res ~new_res:contract.pre in
        let body_usage, _ = compute_resources ~expected_res:contract.post (Some body_res) body in
        match body_usage with
        | Some body_usage -> Var_map.filter (fun _ usage -> usage = Required) body_usage
        | None -> Var_map.empty
      in
      begin match contract with
      | FunSpecContract contract ->
        let body_usage = compute_resources_in_body contract in
        let contract = fun_contract_subst res.aliases contract in
        let args = List.map (fun (x, _) -> x) args in
        (Some body_usage, Some { res with fun_specs = Var_map.add var_result {args; contract; inverse = None} res.fun_specs })
      | FunSpecReverts reverted_fn ->
        (* LATER: allow non empty arg list for reversible functions, this requires subtitution in the reversed contract *)
        assert (args = []);
        (* TODO: Also register reverse in the fun_contracts entry *)
        let reverted_spec = Var_map.find reverted_fn res.fun_specs in
        assert (reverted_spec.args = []);
        let reverse_contract = revert_fun_contract reverted_spec.contract in
        let body_usage = compute_resources_in_body reverse_contract in
        let args = List.map (fun (x, _) -> x) args in
        let fun_specs =
          res.fun_specs |>
          Var_map.add reverted_fn { reverted_spec with inverse = Some var_result } |>
          Var_map.add var_result { args; contract = reverse_contract; inverse = Some reverted_fn }
        in
        (Some body_usage, Some { res with fun_specs })
      | FunSpecUnknown -> (Some Var_map.empty, Some res)
      end

    (* Transitively compute resources through all sequence instructions.
       At the end of the sequence, take into account that all stack allocations are freed. *)
    | Trm_seq instrs ->
      let instrs = Mlist.to_list instrs in
      let usage_map, res = List.fold_left (fun (usage_map, res) inst ->
          compute_resources_and_merge_usage res usage_map inst)
          (Some Var_map.empty, Some res) instrs
      in

      (* Free the cells allocated with stack new *)
      let** res in
      let extract_let_mut ti =
        match trm_let_inv ti with
        | Some (_, x, _, t) ->
          begin match trm_new_inv t with
          | Some (_, [], _) -> [formula_cell x]
          | Some (_, dims, _) -> [formula_matrix (trm_var x) dims]
          | None -> []
          end
        | None -> []
      in
      let to_free = List.concat_map extract_let_mut instrs in
      (*Printf.eprintf "Trying to free %s from %s\n\n" (String.concat ", " to_free) (resources_to_string (Some res));*)
      let res_to_free = Resource_set.make ~linear:(List.map (fun f -> (new_anon_hyp (), formula_uninit f)) to_free) () in
      let _, removed_res, linear = extract_resources ~split_frac:false res res_to_free in
      let usage_map = update_usage_map_opt ~current_usage:usage_map ~extra_usage:(Some (used_set_to_usage_map removed_res)) in

      usage_map, Some { res with linear }

    (* First compute the resources of [body].
       If the body is convertible to a formula, remember it as alias.
       Finally replace all mentions of [var_result] with [var]. *)
    | Trm_let (_, (var, typ), body) ->
      let usage_map, res_after = compute_resources (Some res) body in
      let res_after = Option.map (fun res_after ->
        match formula_of_trm body with
        | Some f_body ->
          let f_body = trm_subst res_after.aliases f_body in
          { res_after with aliases = Var_map.add var f_body res_after.aliases }
        | _ -> res_after
        ) res_after
      in
      usage_map, Option.map (fun res_after -> Resource_set.rename_var var_result var res_after) res_after

    (* TODO: try to factorize *)
    | Trm_apps (fn, effective_args, ghost_args) ->
      begin match find_fun_spec fn res.fun_specs with (* try to find spec *)
      | spec ->
        (* If the function has a specification.
        Check that the [effective_args] use separate resources (order of evaluation does not matter): for now, only deal with read only.
        Check that all [effective_args] do not appear in the function contract unless they are formula-convertible (cf. formula_of_term).
        Build the instantation context with the known [effective_args] and [ghost_args].
        Then [compute_contract_invoc] to deduct used and remaining resources.
        *)

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
        let usage_map = Option.map (fun usage_map ->
          let usage_map = List.fold_left (fun usage_map (_, ghost_inst) ->
              let ghost_inst_fv = trm_free_vars ghost_inst in
              Var_set.fold (fun x -> Hyp_map.add x Required) ghost_inst_fv usage_map
            ) usage_map ghost_args in
          update_usage_map ~current_usage:usage_map ~extra_usage:call_usage_map) usage_map
        in

        usage_map, Some res_after

      | exception Spec_not_found fn when var_eq fn Resource_primitives.__cast ->
        (* TK: we treat cast as identity function. *)
        (* FIXME: this breaks invariant that function arguments are reproducible.
           NOTE: that works if the weaker invariant decribed in apps case above is used. *)
        begin match effective_args with
        | [arg] -> compute_resources (Some res) arg
        | _ -> failwith "expected 1 argument for cast"
        end

      | exception Spec_not_found fn when var_eq fn Resource_trm.var_ghost_begin ->
        (* Checks that the called ghost is reversible, either because the argument is a __with_reverse pair,
           or because its reverse ghost is already in context.
           Then compute ressources as if it is a normal ghost call, and remember the instantiated contract.
           Store the reverse of the instantiated contract as the contract for _Res.
        *)
        let usage_map, res, contract_invoc = Pattern.pattern_match effective_args [
          Pattern.(trm_apps !(trm_apps2 (trm_var (var_eq Resource_trm.var_with_reverse)) !__ !__) nil !__ ^:: nil) (fun with_rev_app ghost_fn ghost_fn_rev ghost_args ->
            let spec = find_fun_spec ghost_fn res.fun_specs in
            let reverse_contract = revert_fun_contract spec.contract in
            begin match trm_fun_inv ghost_fn_rev with
            | Some ([], ret_typ, body, _) ->
              ignore (compute_resources (Some res) (trm_fun ~annot:referent [] ret_typ body ~contract:(FunSpecContract reverse_contract)))
            | Some _ -> failwith "A ghost reverse function should have no arguments"
            | None ->
              ignore (compute_resources (Some reverse_contract.pre) ~expected_res:reverse_contract.post (trm_apps ~annot:referent ghost_fn_rev [] ~ghost_args))
            end;
            let ghost_call = (trm_apps ~annot:referent ghost_fn [] ~ghost_args) in
            let usage_map, res = compute_resources (Some res) ghost_call in
            with_rev_app.ctx <- ghost_call.ctx; (* Recover context information because it can be useful *)
            usage_map, res, ghost_call.ctx.ctx_resources_contract_invoc
          );
          Pattern.(!(trm_apps !__ nil __) ^:: nil) (fun ghost_call ghost_fn ->
            let spec = find_fun_spec ghost_fn res.fun_specs in
            begin match spec.inverse with
            | Some _ -> ()
            | None -> failwith (sprintf "%s is not reversible but is used inside __ghost_begin" (var_to_string fn))
            end;
            let usage_map, res = compute_resources (Some res) ghost_call in
            usage_map, res, ghost_call.ctx.ctx_resources_contract_invoc
          );
          Pattern.(!__) (fun _ -> failwith "expected a ghost call inside __ghost_begin")
        ] in
        begin match res, contract_invoc with
        | Some res, Some invoc ->
          let inverse_pre = List.map (fun { produced_hyp; produced_formula } -> (produced_hyp, produced_formula)) invoc.contract_produced.produced_linear in
          let inverse_post = List.map (fun { pre_hyp; used_formula } -> (pre_hyp, used_formula))
            invoc.contract_inst.used_linear
          in
          let inverse_spec = { args = [];
            contract = fun_contract_subst res.aliases { pre = Resource_set.make ~linear:inverse_pre (); post = Resource_set.make ~linear:inverse_post () };
            inverse = None }
          in
          usage_map, Some { res with fun_specs = Var_map.add var_result inverse_spec res.fun_specs }
        | _ -> failwith "Ghost call inside __ghost_begin should have generated a contract_invoc"
        end

      | exception Spec_not_found fn when var_eq fn Resource_trm.var_ghost_end ->
        (* Calls the closure made by GHOST_BEGIN and removes it from the pure context to ensure good scoping. *)
        Pattern.pattern_match effective_args [
          Pattern.(!(trm_var !__) ^:: nil) (fun fn fn_var ->
            (* LATER: Maybe check that the variable is indeed introduced by __ghost_begin *)
            let usage_map, res = compute_resources (Some res) (trm_apps ~annot:referent fn []) in
            usage_map, Option.map (fun res -> { res with fun_specs = Var_map.remove fn_var res.fun_specs }) res
          );
          Pattern.(!__) (fun _ -> failwith "__ghost_end expects a single variable as argument")
        ]

      | exception Spec_not_found fn when var_eq fn Resource_trm.var_admitted ->
        (* Stop the resource computation in the instructions following the call to __admitted()
           by forgetting the context without raising any error. *)
        None, None
      end

    (* Typecheck the whole for loop by instantiating its outer contract, and type the inside with the inner contract. *)
    | Trm_for (range, body, contract) ->
      (* If there is no contract, put all the resources in a virtual invariant *)
      let contract = match contract with
        | Some contract -> contract
        | None ->
          let invariant_res = Resource_set.make ~linear:(List.map (fun (_, f) -> (new_anon_hyp (), f)) res.linear) () in
          { Resource_contract.empty_loop_contract with invariant = invariant_res }
      in

      let outer_contract = contract_outside_loop range contract in
      let usage_map, res_after = compute_contract_invoc outer_contract res t in

      let inner_contract = contract_inside_loop range contract in
      let inner_usage, _ = compute_resources ~expected_res:inner_contract.post (Some (Resource_set.bind ~keep_efracs:false ~old_res:res ~new_res:inner_contract.pre)) body in

      let usage_map = Option.map (fun inner_usage -> update_usage_map ~current_usage:usage_map ~extra_usage:(Var_map.filter (fun _ usage -> usage = Required) inner_usage)) inner_usage in
      usage_map, Some res_after

    (* Pass through constructions that do not interfere with resource checking *)
    | Trm_typedef _ ->
      Some Var_map.empty, Some res

    | Trm_template (params, t) ->
      compute_resources (Some res) t

    | _ -> trm_fail t ("Resource_core.compute_inplace: not implemented for " ^ AstC_to_c.ast_to_string t)
    end with
    | StoppedOnFirstError as e -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
    | e -> handle_resource_errors t ResourceComputation e
  in

  t.ctx.ctx_resources_usage <- usage_map;
  if debug_print_computation_stack then Printf.eprintf "=====\nWith resources: %s\nSaving %s\n\n" (resource_set_opt_to_string res) (AstC_to_c.ast_to_string t);
  t.ctx.ctx_resources_after <- res;
  let res =
    match res, expected_res with
    | Some res, Some expected_res ->
      (* Check that the expected resources after the expression are actually the resources available after the expression *)
      begin try
        (* TODO: check names *)
        let used_res = assert_resource_impl res expected_res in
        t.ctx.ctx_resources_post_inst <- Some used_res
      with
      | StoppedOnFirstError as e -> Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
      | e -> ignore (handle_resource_errors t PostconditionCheck e)
      end;
      Some expected_res
    | _, None -> res
    | None, _ -> expected_res
  in
  usage_map, res

(** <private> [compute_resources] that propagates usages. *)
and compute_resources_and_merge_usage
  ?(expected_res: resource_set option) (res: resource_set option)
  (current_usage: resource_usage_map option) (t: trm): resource_usage_map option * resource_set option =
  let extra_usage, res = (compute_resources : ?expected_res:resource_set -> resource_set option -> trm -> (resource_usage_map option * resource_set option)) ?expected_res res t in
  let open Xoption.OptionMonad in
  let usage_map = update_usage_map_opt ~current_usage ~extra_usage in
  (usage_map, res)


let rec trm_deep_copy (t: trm) : trm =
  let t = trm_map_with_terminal ~share_if_no_change:false false (fun _ ti -> trm_deep_copy ti) t in
  t.ctx <- unknown_ctx (); (* LATER *)
  t

(** [trm_recompute_resources init_ctx t] recomputes resources of [t] using [compute_resources],
  after a [trm_deep_copy] to prevent sharing.
  Otherwise, returns a fresh term in case of success, or raises [ResourceError] in case of failure.

  If [!Flags.stop_on_first_resource_error] is set, then the function immediately stops after the
  first error is encountered, else it tries to report as many as possible.

  Hypothesis: needs var_ids to be calculated. *)
let trm_recompute_resources (init_ctx: resource_set) (t: trm): trm =
  (* TODO: should we avoid deep copy by maintaining invariant that there is no sharing?
     makes sense with unique var ids and trm_copy mechanisms.
     Otherwise avoid mutable asts. Could also have unique node ids and maps from ids to metadata. *)
  (* Make a copy of [t] *)
  let t = trm_deep_copy t in
  (* Typecheck the copy of [t] and update it in place.
     Type errors are saved in a global list, and errors
     are also saved as annotations in the "errors" fields
     in the nodes inside the copy of [t]. *)
  global_errors := [];
  let backtrace =
    try
      ignore (compute_resources (Some init_ctx) t);
      None
    with StoppedOnFirstError -> Some (Printexc.get_raw_backtrace ())
  in
  (* Test for errors, before returning the updated copy of [t] *)
  match !global_errors with
  | [] -> t
  | ((phase, exn) :: _) ->
      let err = ResourceError (t, phase, exn) in
      match backtrace with
      | Some bt -> Printexc.raise_with_backtrace err bt
      | None -> raise err
