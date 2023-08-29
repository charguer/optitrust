open Syntax
open Resources_contract

type pure_resource_set = resource_item list
type linear_resource_set = resource_item list

(* The built-in variable representing a function's return value. *)
(* FIXME: #var-id, id should change *)
let var_result = toplevel_free_var "_Res"
let trm_result: formula = trm_var var_result
let _Full = toplevel_free_var "_Full"

(* The contract of the [set] function. *)
let set_fun_contract p =
  { pre = resource_set ~linear:[(new_anon_hyp (), formula_cell p)] ();
    post = resource_set ~linear:[(new_anon_hyp (), formula_cell p)] (); }

(* LATER: express these as parsed C function definitions *)
let __cast = toplevel_free_var "__cast"
let __new = toplevel_free_var "__new"
let __get = toplevel_free_var "__get"
let __set = toplevel_free_var "__set"
let __add = toplevel_free_var "__add"
let __sub = toplevel_free_var "__sub"
let __mul = toplevel_free_var "__mul"
let __array_access = toplevel_free_var "__array_access"
let __add_inplace = toplevel_free_var "__add_inplace"
let __sub_inplace = toplevel_free_var "__sub_inplace"
let __mul_inplace = toplevel_free_var "__mul_inplace"

(* The environment containing the contracts of builtin functions. *)
let builtin_env =
  let h x = new_hyp x in
  let p1 = h "p" in let p2 = h "p" in
  let p3 = h "p" in let p4 = h "p" in
  let p5 = h "p" in
  resource_set ~fun_contracts:(var_map_of_list [
    __new, ([h "init"],
      { pre = empty_resource_set;
        post = resource_set ~linear:[(new_anon_hyp (), formula_cell var_result)] () });
    __get, ([p1],
      push_read_only_fun_contract_res (None, formula_cell p1) empty_fun_contract);
    __set, ([p2; h "x"], set_fun_contract p2);
    __add, ([h "x1"; h "x2"], empty_fun_contract);
    __sub, ([h "x1"; h "x2"], empty_fun_contract);
    __mul, ([h "x1"; h "x2"], empty_fun_contract);
    __array_access, ([h "tab"; h "i"], empty_fun_contract);
    __add_inplace, ([p3; h "x"], set_fun_contract p3);
    __sub_inplace, ([p4; h "x"], set_fun_contract p4);
    __mul_inplace, ([p5; h "x"], set_fun_contract p5);
  ]) ()

(* A formula that may instantiate contract variables with
   hypotheses from the calling context.
   When instantating a contract with X := Y substitutions,
   Y is a formula_inst.
  *)
type formula_inst = formula

let inst_hyp (h: hyp): formula_inst =
  trm_make (Trm_var (Var_immutable, h))

let inst_hyp_inv (f: formula_inst) =
  match f.desc with
  | Trm_var (Var_immutable, h) -> Some h
  | _ -> None

let var_SplitRO = toplevel_free_var "SplitRO"

let inst_split_read_only ~(new_frac: var) (h: hyp) : formula_inst =
  trm_apps (trm_var var_SplitRO) [trm_var new_frac; inst_hyp h]

let inst_split_read_only_inv (f: formula_inst): (var * hyp) option =
  let open Tools.OptionMonad in
  match trm_apps_inv f with
  | Some (fn, [frac; hyp]) ->
    begin match trm_var_inv fn with
    | Some v when var_eq v var_SplitRO ->
      let* frac = trm_var_inv frac in
      let* hyp = inst_hyp_inv hyp in
      Some (frac, hyp)
    | _ -> None
    end
  | _ -> None

let subst_in_resources ?(forbidden_binders = Var_set.empty) (subst_map: tmap) (res: resource_set): tmap * resource_set =
  let subst_var_in_resource_list =
    List.fold_left_map (fun subst_ctx (h, t) ->
        let t = trm_subst subst_map t in
        (subst_ctx, (h, t))
      )
  in
  let subst_ctx, pure = subst_var_in_resource_list (forbidden_binders, subst_map) res.pure in
  let _, linear = subst_var_in_resource_list subst_ctx res.linear in
  (snd subst_ctx, { pure; linear;
    fun_contracts = res.fun_contracts (* TODO: subst here as well? *) })

let subst_var_in_resources (x: var) (t: trm) (res: resource_set) : resource_set =
  snd (subst_in_resources (Var_map.singleton x t) res)

let rename_var_in_resources (x: var) (new_x: var) (res: resource_set) : resource_set =
  subst_var_in_resources x (trm_var new_x) res


let fun_contract_free_vars (contract: fun_contract): Var_set.t =
  let fold_res_list bound_vars fv res =
    List.fold_left (fun (bound_vars, fv) (h, formula) ->
      let bound_vars = Var_set.add h bound_vars in
      (bound_vars, Var_set.union (trm_free_vars ~bound_vars formula) fv)) (bound_vars, fv) res
  in
  let bound_vars, free_vars = fold_res_list Var_set.empty Var_set.empty contract.pre.pure in
  let _, free_vars = fold_res_list bound_vars free_vars contract.pre.linear in
  let bound_vars, free_vars = fold_res_list bound_vars free_vars contract.post.pure in
  let _, free_vars = fold_res_list bound_vars free_vars contract.post.linear in
  free_vars

(* [Resource_not_found (item, res_list)]: exception raised when the resource
   [item] is not found inside the resource list [res_list] *)
exception Resource_not_found of resource_item * resource_item list

let raise_resource_not_found ((name, formula): resource_item) (evar_ctx: unification_ctx) (inside: resource_item list) =
  let subst_ctx = Var_map.mapi (fun var subst ->
    match subst with Some t -> t | None -> trm_var { var with name = ("?" ^ var.name) }
  ) evar_ctx in
  let formula = trm_subst subst_ctx formula in
  raise (Resource_not_found ((name, formula), inside))

(* Unify the given resource_item with one of the resources in the pure resource set.
   Also add a binding from x to the found resource in evar_ctx.
   If it fails raise a Resource_not_found exception. *)
let unify_pure ((x, formula): resource_item) (res: pure_resource_set) (evar_ctx: unification_ctx): unification_ctx =
  (* Add flag to disallow pure instantiation *)
  let find_formula formula (hyp_candidate, formula_candidate) =
    Option.map (fun evar_ctx -> Var_map.add x (Some (trm_var hyp_candidate)) evar_ctx)
      (unify_trm formula_candidate formula evar_ctx)
  in
  match List.find_map (find_formula formula) res with
  | Some evar_ctx -> evar_ctx
  | None -> raise_resource_not_found (x, formula) evar_ctx res

let rec unify_and_remove_linear ((x, formula): resource_item) (res: linear_resource_set)
  (evar_ctx: unification_ctx): used_resource_item * linear_resource_set * unification_ctx =
  (* LATER: Improve the structure of the linear_resource_set to make this
     function faster on most frequent cases *)
  let aux res = unify_and_remove_linear (x, formula) res evar_ctx in
  match res with
  | [] -> raise Not_found (* caught later to create a Resource_not_found. *)
  | (candidate_name, formula_candidate) as hyp_candidate :: res ->
    match unify_trm formula_candidate formula evar_ctx with
    | Some evar_ctx -> ({ hyp_to_inst = x; inst_by = inst_hyp candidate_name; used_formula = formula_candidate }, res, evar_ctx)
    | None ->
      let used, res, evar_ctx = aux res in
      (used, hyp_candidate :: res, evar_ctx)

let rec unify_and_split_read_only (hyp_to_inst: hyp) ~(new_frac: var) (formula: formula) (res: linear_resource_set)
  (evar_ctx: unification_ctx): used_resource_item * linear_resource_set * unification_ctx =
  (* LATER: Improve the structure of the linear_resource_set to make this
     function faster on most frequent cases *)
  let aux res = unify_and_split_read_only hyp_to_inst ~new_frac formula res evar_ctx in
  match res with
  | [] -> raise Not_found (* caught later to create a Resource_not_found. *)
  | (h, formula_candidate) as hyp_candidate :: res ->
    let cur_frac, formula_candidate = match formula_read_only_inv formula_candidate with
      | Some { frac; formula } -> frac, formula
      | None -> full_frac, formula_candidate
    in
    match unify_trm formula_candidate formula evar_ctx with
    | Some evar_ctx ->
      ({ hyp_to_inst ; inst_by = inst_split_read_only ~new_frac h; used_formula = formula_read_only ~frac:(trm_var new_frac) formula_candidate },
       (h, formula_read_only ~frac:(trm_sub cur_frac (trm_var new_frac)) formula_candidate) :: res, evar_ctx)
    | None ->
      let used, res, evar_ctx = aux res in
      (used, hyp_candidate :: res, evar_ctx)

(* FIXME: explain relationship to unify_and_remove_linear. explain [split_frac] somewhere. *)
let subtract_linear_resource_item ~(split_frac: bool) ((x, formula): resource_item) (res: linear_resource_set)
  (evar_ctx: unification_ctx): used_resource_item * linear_resource_set * unification_ctx =
  try match formula_read_only_inv formula with
    | Some { frac; formula = ro_formula } when split_frac ->
      begin match trm_apps_inv frac with
      | Some (maybe_full, [frac]) when Option.equal var_eq (trm_var_inv maybe_full) (Some _Full) ->
        unify_and_remove_linear (x, formula_read_only ~frac ro_formula) res evar_ctx
      | _ ->
        begin match trm_var_inv frac with
        | Some frac_var ->
          begin match Var_map.find_opt frac_var evar_ctx with
          | Some None ->
            let new_frac, _ = new_frac () in (* LATER: store new generated frac ghosts *)
            let evar_ctx = Var_map.add frac_var (Some (trm_var new_frac)) evar_ctx in
            unify_and_split_read_only x ~new_frac ro_formula res evar_ctx
          | _ ->
            unify_and_remove_linear (x, formula) res evar_ctx
          end
        | _ -> unify_and_remove_linear (x, formula) res evar_ctx
        end
      end
    | _ -> unify_and_remove_linear (x, formula) res evar_ctx
  with Not_found ->
    raise_resource_not_found (x, formula) evar_ctx res

(* [subtract_linear_resource]: subtract [res_removed] from [res_from].
   Raise [Resource_not_found] if one resource is missing.
   Use the unification environment [evar_ctx] for all resources in [res_removed]
   potentially instantiating evars inside.
   If [split_frac] is true, always try to give a smaller fraction than what is
   inside [res_from] for evar fractions.
*)
let subtract_linear_resource ~(split_frac: bool) (res_from: linear_resource_set) (res_removed: linear_resource_set)
  (evar_ctx: unification_ctx) : used_resource_item list * linear_resource_set * unification_ctx =
  List.fold_left (fun (used_list, res_from, evar_ctx) res_item ->
      let used, res_from, evar_ctx = subtract_linear_resource_item ~split_frac res_item res_from evar_ctx in
      (used :: used_list, res_from, evar_ctx)
    ) ([], res_from, evar_ctx) res_removed

(* Eliminates dominated evars from [res], returning the dominated evars and the remaining resources.
   An evar is dominated if its value will be implied by the instantation of other resources (i.e. it appears in the formula of another resource). *)
let eliminate_dominated_evars (res: resource_set): resource_set * var list =
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
  ({ res with pure }, !dominated_evars)

exception Spec_not_found of var
exception NotConsumedResources of linear_resource_set
exception ImpureFunctionArgument of exn

(* previous name: resource_impl_leftovers *)
(* [extract_resources]: checks that [res_from] ==> [res_to] * [H] in
   separation logic and return the leftover linear resources [H] along with
   the substitution context after instantiating ghost variables in [res_to]:
   effectively, this checks that all resources inside [res_to] can be built
   from resources inside [res_from] and returns the remaining linear resources
   after instantiation.
   Pure resources (ghosts) can be inferred using unification.

   If given [evar_ctx] is used as an initial unification context, and replaces
   the context inferred from pure resources. In that case, pure ressources must
   already be filtered out of [res_to].

   TODO: Add unit tests for this specific function
*)
let rec extract_resources ~(split_frac: bool) (res_from: resource_set) ?(subst_ctx: tmap = Var_map.empty) (res_to: resource_set) : tmap * used_resource_set * linear_resource_set =
  let remaining_res_to, dominated_evars = eliminate_dominated_evars res_to in
  let evar_ctx = List.fold_left (fun evar_ctx x -> Var_map.add x None evar_ctx)
      (Var_map.map (fun x -> Some x) subst_ctx) dominated_evars
  in

  let used_linear, leftover_linear, evar_ctx = subtract_linear_resource ~split_frac res_from.linear res_to.linear evar_ctx in
  let evar_ctx = List.fold_left (fun evar_ctx res_item ->
      unify_pure res_item res_from.pure evar_ctx) evar_ctx remaining_res_to.pure
  in

  (* All unifications should be done at this point. There is a bug if it's not the case. *)
  let subst_ctx = Var_map.map (function
      | Some t -> t
      | None -> failwith "failed unification") evar_ctx
  in

  let used_pure = List.map (fun (hyp, formula) ->
      { hyp_to_inst = hyp; inst_by = Var_map.find hyp subst_ctx; used_formula = trm_subst subst_ctx formula }
    ) res_to.pure in

  (* TODO: what is this? *)
  ignore (Var_map.merge
            (fun fn_name spec_from spec_to ->
              match spec_from, spec_to with
              | _, None -> None (* We can drop extra specifications *)
              | None, Some _ -> raise (Spec_not_found fn_name)
              | Some spec_from, Some spec_to ->
                if spec_from = spec_to
                  then None
                  else failwith "extract_resources: Unimplemented complex contract implications"
            )
            res_from.fun_contracts res_to.fun_contracts);

  (subst_ctx, { used_pure; used_linear }, leftover_linear)

(* FIXME: resource set intuition breaks down, should we talk about resource predicates? *)
(* [assert_resource_impl]: checks that [res_from] ==> [res_to] *)
and assert_resource_impl (res_from: resource_set) (res_to: resource_set) : used_resource_set =
  let _, used_res, leftovers = extract_resources ~split_frac:false res_from res_to in
  if leftovers <> [] then raise (NotConsumedResources leftovers);
  used_res

(* Computes the resources produced by [contract_post] given the [subst_ctx] instantation of the contract. *)
let compute_produced_resources (subst_ctx: tmap) (contract_post: resource_set) : produced_resource_set =
  let compute_produced_resources_list =
    List.fold_left_map (fun subst_ctx (h, formula) ->
        let produced_hyp = new_anon_hyp () in
        let produced_formula = trm_subst subst_ctx formula in
        let produced = { produced_hyp; produced_from = h; produced_formula } in
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
  { pure; linear; fun_contracts = Var_map.empty }

let rec trm_keep_only_desc (t: trm): trm =
  let t =
    match t.desc with
    | Trm_var (_, qx) -> trm_var qx
    | t -> trm_make t
  in
  trm_map_with_terminal_unopt false (fun _ -> trm_keep_only_desc) t

(* [resource_merge_after_frame]:
 * Returns [res_after] * [frame] with simplifications.
 * Cancels magic wands in [frame] with linear resources in [res_after] and
 * returns the produced resource_set.
 *
 * Ex: res_after.linear = RO('a, t)  and  frame = RO('b - 'a, t)
 * gives res.linear = RO('b, t)
 *)
let resource_merge_after_frame (res_after: produced_resource_set) (frame: linear_resource_set) : resource_set =
  let res_after = produced_resources_to_resource_set res_after in
  let ro_formulas = Hashtbl.create (List.length res_after.linear) in
  (* Accumulate into ro_formulas the pairs ('a, t) when res_after.linear contains RO('a, t) *)
  List.iter (fun (_, formula) ->
      match formula_read_only_inv formula with
      | Some { frac; formula = ro_formula } ->
        begin match trm_var_inv frac with
        | Some frac_var -> Hashtbl.add ro_formulas (trm_keep_only_desc ro_formula, frac_var) ()
        | None -> ()
        end
      | None -> ()
    ) res_after.linear;

  let rec reunite_fracs frac ro_formula =
    match trm_binop_inv Binop_sub frac with
    | Some (sub_frac, frac_atom) ->
      let sub_frac = reunite_fracs sub_frac ro_formula in
      let ro_formula_erased = trm_keep_only_desc ro_formula in
      begin match trm_var_inv frac_atom with
      | Some atom when Hashtbl.mem ro_formulas (ro_formula_erased, atom) ->
        Hashtbl.remove ro_formulas (ro_formula_erased, atom);
        sub_frac
      (* DEBUG:
      | Some atom ->
        Printf.eprintf "Failed to find %s\n" (var_to_string atom);
        Printf.eprintf "%s\n" (AstC_to_c.ast_to_string ro_formula);
        Hashtbl.iter (fun (a, _) () -> Printf.eprintf "%b -- %s\n" (a = ro_formula) (AstC_to_c.ast_to_string a)) ro_formulas;
        trm_sub sub_frac frac_atom *)
      | _ -> trm_sub sub_frac frac_atom
      end
    | None -> frac
  in
  let frame = List.map (fun (x, formula) ->
      match formula_read_only_inv formula with
      | Some { frac; formula = ro_formula } ->
        let frac = reunite_fracs frac (trm_keep_only_desc ro_formula) in
        if frac = full_frac
          then (x, ro_formula)
          else (x, formula_read_only ~frac ro_formula)
      | None -> (x, formula)
    ) frame in

  let linear = List.fold_left (fun acc (h, formula) ->
      match formula_read_only_inv formula with
      | Some { frac; formula = ro_formula } ->
        let ro_formula_erased = trm_keep_only_desc ro_formula in
        (* DEBUG
        Hashtbl.iter (fun (a, _) () -> Printf.eprintf "%b -- %s\n" (a = ro_formula_erased) (AstC_to_c.ast_to_string a)) ro_formulas;
        Printf.eprintf "formula: %s\n" (AstC_to_c.ast_to_string ro_formula_erased); *)
        begin match trm_var_inv frac with
        | Some frac when Hashtbl.mem ro_formulas (ro_formula_erased, frac) ->
          Hashtbl.remove ro_formulas (ro_formula_erased, frac);
          (h, formula) :: acc
        | Some frac -> acc (* Consumed ro_formula *)
        | None -> (h, formula) :: acc
        end
      | None -> (h, formula) :: acc) frame res_after.linear in
  assert (Hashtbl.length ro_formulas = 0);
  { res_after with linear }

(* [bind_new_resources]: Add new pure resources to the old ones and replace linear resources. *)
let bind_new_resources ~(old_res: resource_set) ~(new_res: resource_set): resource_set =
  { pure = new_res.pure @ old_res.pure;
    linear = new_res.linear;
    fun_contracts = Var_map.union (fun _ new_c _ -> Some new_c) new_res.fun_contracts old_res.fun_contracts }

let resource_names (res: resource_set) : Var_set.t =
  let res_list_names (res: resource_item list) =
    List.fold_left (fun avoid_names (h, _) ->
            Var_set.add h avoid_names) Var_set.empty res
  in
  Var_set.union (res_list_names res.pure) (res_list_names res.linear)

let cast_into_read_only (res: resource_set): resource_set =
  let frac_var, _ = new_frac () in
  let frac = trm_var frac_var in
  let linear = List.map (fun (x, formula) ->
      match formula_read_only_inv formula with
      | Some _ -> (x, formula)
      | None -> (x, formula_read_only ~frac formula)) res.linear in
  { res with linear }

exception Unimplemented

let unop_to_var_name (u: unary_op): string =
  match u with
  | Unop_get -> "__get"
  | Unop_cast t -> "__cast"
  | _ -> raise Unimplemented

let binop_to_var_name (u: binary_op): string =
  match u with
  | Binop_add -> "__add"
  | Binop_sub -> "__sub"
  | Binop_mul -> "__mul"
  | Binop_array_access -> "__array_access"
  | Binop_set -> "__set"
  | _ -> raise Unimplemented

let prim_to_var (p: prim): var =
  toplevel_free_var (match p with
  | Prim_unop u -> unop_to_var_name u
  | Prim_binop b -> binop_to_var_name b
  | Prim_compound_assgn_op b -> (binop_to_var_name b ^ "_inplace")
  | Prim_new _ -> "__new"
  | _ -> raise Unimplemented)

let trm_fun_var_inv (t:trm): var option =
  try
    match trm_prim_inv t with
    | Some p -> Some (prim_to_var p)
    | None -> trm_var_inv t
  with Unimplemented ->
    let trm_internal (msg : string) (t : trm) : string =
      Printf.sprintf "%s: %s\n" msg (Ast_to_text.ast_to_string t) in
    fail t.loc (trm_internal "unimplemented trm_fun_var_inv construction" t)

let resources_to_string res : string =
  match res with
  | Some res ->
  let spure = Ast_fromto_AstC.ctx_resource_list_to_string res.pure in
  let slin = Ast_fromto_AstC.ctx_resource_list_to_string res.linear in
  Printf.sprintf "pure = %s | linear = %s" spure slin
  | None -> "UnspecifiedRes"

type resource_error_phase = ResourceComputation | ResourceCheck
exception ResourceError of location * resource_error_phase * exn

let _ = Printexc.register_printer (function
  | Resource_not_found (item, context) ->
    Some (Printf.sprintf "Resource '%s' not found in context '%s'" (Ast_fromto_AstC.named_formula_to_string item) (Ast_fromto_AstC.ctx_resource_list_to_string context))
  | Spec_not_found fn ->
    Some (Printf.sprintf "No specification for function %s" (var_to_string fn))
  | NotConsumedResources res ->
    Some (Printf.sprintf "Resources not consumed after the end of the block: '%s'" (Ast_fromto_AstC.ctx_resource_list_to_string res))
  | ImpureFunctionArgument err ->
    Some (Printf.sprintf "Function argument subexpression resource preservation check failed: %s" (Printexc.to_string err))
  | ResourceError (loc, ResourceComputation, err) ->
    Some (Printf.sprintf "%s: Resource computation error: %s" (loc_to_string loc) (Printexc.to_string err));
  | ResourceError (loc, ResourceCheck, err) ->
    Some (Printf.sprintf "%s: Resource check error: %s" (loc_to_string loc) (Printexc.to_string err))
  | _ -> None)

(* LATER: Extensible list of applications that can be translated into formula.
   OR A term can be embedded in a formula if it is pure (no linear resources).
   Does [t] change or is this just checking whether trm_is_formula?
   I.e. are the trm and formula languages intersecting or separate?
   *)
let rec formula_of_trm (t: trm): formula option =
  let open Tools.OptionMonad in
  match t.desc with
  | Trm_val _ | Trm_var _ -> Some t
  | Trm_apps (fn, args) ->
    let* f_args = try Some (List.map (fun arg -> Option.get (formula_of_trm arg)) args) with Invalid_argument _ -> None in
    begin match trm_prim_inv fn with
      | Some Prim_binop Binop_add
      | Some Prim_binop Binop_sub
      | Some Prim_binop Binop_mul
      | Some Prim_binop Binop_div
      | Some Prim_binop Binop_mod
      | Some Prim_binop Binop_array_access
          -> Some (trm_apps fn f_args)
      | Some _ -> None
      | None ->
        begin match trm_var_inv fn with
          | Some fv when String.starts_with ~prefix:"MINDEX" fv.name
            -> Some (trm_apps fn f_args)
          | _ -> None
        end
    end
  | _ -> None

let empty_usage_map (res: resource_set): resource_usage_map =
 List.fold_left (fun acc (h, _) -> Hyp_map.add h NotUsed acc) Hyp_map.empty res.linear

let update_usage_map ~(current_usage: resource_usage_map) ~(child_usage: resource_usage_map): resource_usage_map =
  Hyp_map.merge (fun _ cur_use ch_use ->
      match cur_use, ch_use with
      | None, _ -> None
      | Some u, None -> Some u
      | Some NotUsed, Some u -> Some u
      | Some u, Some NotUsed -> Some u
      | Some UsedReadOnly, Some u -> Some u
      | Some u, Some UsedReadOnly -> Some u
      | Some UsedFull, Some UsedFull -> Some UsedFull
    ) current_usage child_usage

let update_usage_map_opt ~(current_usage: resource_usage_map option) ~(child_usage: resource_usage_map option): resource_usage_map option =
  let open Tools.OptionMonad in
  let* current_usage in
  let* child_usage in
  Some (update_usage_map ~current_usage ~child_usage)

let add_used_set_to_usage_map (res_used: used_resource_set) (usage_map: resource_usage_map) : resource_usage_map =
  (* TODO: Maybe manage pure as well *)
  let locally_used =
    List.fold_left (fun usage_map { inst_by } ->
      match inst_split_read_only_inv inst_by with
      | Some (_, orig_hyp) -> Hyp_map.add orig_hyp UsedReadOnly usage_map
      | None ->
        match inst_hyp_inv inst_by with
        | Some hyp -> Hyp_map.add hyp UsedFull usage_map
        | None -> failwith "Weird resource used"
    ) Hyp_map.empty res_used.used_linear
  in
  update_usage_map ~current_usage:usage_map ~child_usage:locally_used

let debug_print_computation_stack = false

(* TODO?
Resources.Computation.compute_resource
Resources.compute = Resources.Computation.compute_resource
when dune supports this.
*)
(* compute_resource = compute_resources_and_merge_usage ~current_usage:(empty_usage_map res) *)
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
    let usage_map = empty_usage_map res in
    try begin match t.desc with
    | Trm_val _ | Trm_var _ -> (Some usage_map, Some res) (* TODO: Manage return values for pointers *)

    | Trm_let_fun (name, ret_type, args, body, contract) ->
      begin match contract with
      | Some contract ->
        let body_res = bind_new_resources ~old_res:res ~new_res:contract.pre in
        (* LATER: Merge used pure facts *)
        ignore (compute_resources ~expected_res:contract.post (Some body_res) body);
        let args = List.map (fun (x, _) -> x) args in
        (Some usage_map, Some { res with fun_contracts = Var_map.add name (args, contract) res.fun_contracts })
      | None -> (Some usage_map, Some res)
      end

    | Trm_seq instrs ->
      let instrs = Mlist.to_list instrs in
      let usage_map, res = List.fold_left (fun (usage_map, res) inst ->
          compute_resources_and_merge_usage res usage_map inst)
          (Some usage_map, Some res) instrs
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
        let res_to_free = resource_set ~linear:(List.map (fun x -> (new_anon_hyp (), formula_cell x)) to_free) () in
        let _, _, linear = extract_resources ~split_frac:false res res_to_free in
        { res with linear }) res
      in
      usage_map, res

    | Trm_let (_, (var, typ), body, spec) ->
      begin match spec with
      | Some bound_res ->
        (* FIXME: This breaks usage_map because it allows renaming without using the renamed hypothesis *)
        let expected_res = rename_var_in_resources var var_result bound_res in
        let usage_map, _ = compute_resources_and_merge_usage ~expected_res (Some res) (Some usage_map) body in
        (* Use the bound_res contract but keep res existing pure facts *)
        usage_map, Some (bind_new_resources ~old_res:res ~new_res:bound_res)
      | None ->
        let usage_map, res_after = compute_resources_and_merge_usage (Some res) (Some usage_map) body in
        usage_map, Option.map (fun res_after -> rename_var_in_resources var_result var res_after) res_after
      end

    | Trm_apps (fn, effective_args) ->
      let fn = trm_inv ~error:"Calling an anonymous function that is not bound to a variable is unsupported" trm_fun_var_inv fn in
      begin match Var_map.find_opt fn res.fun_contracts with
      | Some (contract_args, contract) ->
        (* The arguments of the function call cannot have write effects, and cannot be referred to in the function contract unless they are formula-convertible (cf. formula_of_term). *)
        (* TODO: Cast into readonly: better error message when a resource exists in RO only but asking for RW *)
        let read_only_res = cast_into_read_only res in
        let contract_fv = fun_contract_free_vars contract in
        let subst_ctx, usage_map = try
          List.fold_left2 (fun (subst_map, usage_map) contract_arg effective_arg ->
            (* Give resources as read only and check that they are still there after the argument evaluation *)
            (* LATER: Collect pure facts of arguments:
               f(3+i), f(g(a)) where g returns a + a, f(g(a)) where g ensures res mod a = 0 *)
            let usage_map, post_arg_res = compute_resources_and_merge_usage (Some read_only_res) usage_map effective_arg in
            begin match post_arg_res with
            | Some post_arg_res ->
              begin try ignore (assert_resource_impl post_arg_res (resource_set ~linear:read_only_res.linear ()))
              with e -> raise (ImpureFunctionArgument e)
              end
            | None -> ()
            end;

            if Var_set.mem contract_arg contract_fv then begin
              let arg_formula = match formula_of_trm effective_arg with
                | Some formula -> formula
                | None -> fail effective_arg.loc (Printf.sprintf "Could not make a formula out of term '%s', required because of instantiation of %s contract" (AstC_to_c.ast_to_string effective_arg) (var_to_string fn))
              in
              Var_map.add contract_arg arg_formula subst_map, usage_map
            end else
              subst_map, usage_map
          ) (Var_map.empty, Some usage_map) contract_args effective_args;
        with Invalid_argument _ ->
          failwith (Printf.sprintf "Mismatching number of arguments for %s" (var_to_string fn))
        in

        let subst_ctx, res_used, res_frame = extract_resources ~split_frac:true ~subst_ctx res contract.pre in

        let usage_map = Option.map (fun usage_map -> add_used_set_to_usage_map res_used usage_map) usage_map in

        let res_produced = compute_produced_resources subst_ctx contract.post in
        t.ctx.ctx_resources_contract_invoc <- Some {
            contract_frame = res_frame;
            contract_inst = res_used;
            contract_produced = res_produced };

        usage_map, Some (bind_new_resources ~old_res:res ~new_res:(resource_merge_after_frame res_produced res_frame))

      | None when var_eq fn __cast ->
        (* TK: we treat cast as identity function. *)
        (* FIXME: this breaks invariant that function arguments are pure/reproducible. *)
        begin match effective_args with
        | [arg] -> compute_resources_and_merge_usage (Some res) (Some usage_map) arg
        | _ -> failwith "expected 1 argument for cast"
        end
      | None when fn.name = "__admitted" ->
        (* LATER: Force the function __admitted inside optitrust.h to have a predefined id to make this check faster and allow proper shadowing *)
        None, None
      | None -> raise (Spec_not_found fn)
      end

    | Trm_for (range, body, None) ->
      (* If no spec is given, put all the resources in the invariant (best effort) *)
      (* TODO: Still try to be clever about Group with a corresponding range *)
      let expected_res = resource_set ~linear:res.linear () in
      let usage_map, _ = compute_resources_and_merge_usage ~expected_res (Some res) (Some usage_map) body in
      usage_map, Some res

    | Trm_for ((index, tstart, _, tend, step, _) as range, body, Some contract) ->
      (* Compute resources outside the loop *)
      let invariant_before = subst_var_in_resources index tstart contract.invariant in
      let before_loop_res = res_union invariant_before (res_group_range range contract.iter_contract.pre) in
      let before_loop_res = { before_loop_res with pure = contract.loop_ghosts @ before_loop_res.pure } in
      let ghost_subst_ctx, res_used, res_frame = extract_resources ~split_frac:true res before_loop_res in

      (* Compute resources inside the loop body *)
      let loop_body_pre = res_union contract.invariant contract.iter_contract.pre in
      let loop_body_pre = { loop_body_pre with pure = contract.loop_ghosts @ loop_body_pre.pure; fun_contracts = res.fun_contracts } in
      let invariant_after_one_iter = subst_var_in_resources index (trm_add (trm_var index) (loop_step_to_trm step)) contract.invariant in
      let loop_body_post = res_union invariant_after_one_iter contract.iter_contract.post in
      ignore (compute_resources ~expected_res:loop_body_post (Some loop_body_pre) body);

      let after_loop_res = res_union contract.invariant (res_group_range range contract.iter_contract.post) in
      let after_loop_res = compute_produced_resources (Var_map.add index tend ghost_subst_ctx) after_loop_res in

      t.ctx.ctx_resources_contract_invoc <- Some { contract_frame = res_frame; contract_inst = res_used; contract_produced = after_loop_res };

      let usage_map = add_used_set_to_usage_map res_used usage_map in

      Some usage_map, Some (bind_new_resources ~old_res:res ~new_res:(resource_merge_after_frame after_loop_res res_frame))

    | Trm_typedef _ ->
      Some usage_map, Some res

    | _ -> fail t.loc ("Resource_core.compute_inplace: not implemented for " ^ AstC_to_c.ast_to_string t)
    end with e when !Flags.resource_errors_as_warnings ->
      Printf.eprintf "%s: Resource computation warning: %s\n" (loc_to_string t.loc) (Printexc.to_string e);
      None, None
    | ResourceError (None, place, err) -> raise (ResourceError (t.loc, place, err))
    | ResourceError (Some _, _, _) as e -> raise e
    | e -> Printexc.(raise_with_backtrace (ResourceError (t.loc, ResourceComputation, e)) (get_raw_backtrace ()))
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
  let child_usage, res = compute_resources ?expected_res res t in
  let usage_map = update_usage_map_opt ~current_usage ~child_usage in
  (usage_map, res)

let ctx_copy (ctx: ctx): ctx = { ctx with ctx_types = ctx.ctx_types }

let rec trm_deep_copy (t: trm) : trm =
  let t = trm_map_with_terminal_unopt false (fun _ ti -> trm_deep_copy ti) t in
  t.ctx <- ctx_copy unknown_ctx; (* LATER *)
  t

(* hypothesis: needs var_ids to be calculated *)
let trm_recompute_resources (init_ctx: resource_set) (t: trm): trm =
  let t = trm_deep_copy t in
  ignore (compute_resources (Some init_ctx) t);
  t
