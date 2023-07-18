open Ast
open Resources_contract

type pure_resource_set = resource_item list
type linear_resource_set = resource_item list

(* The built-in variable representing a function's return value. *)
let var_result = "_Res"
let trm_result: formula = trm_var var_result

(* The value associated with an existential variable (evar).
   None if the value is unknown, Some if the value is known (it was unified). *)
type eval = trm option

(* The unification context is a map from variables to evals.
   If a variable is not in the map, then it is not an evar, and should not be substituted/unified.
   If a variable is in the map, then it is an evar, and should be substituted/unified (i.e. its eval should eventually become Some). *)
type unification_ctx = eval varmap

(* The contract of the [set] function. *)
let set_fun_contract =
  { pre = resource_set ~linear:[(new_anon_hyp (), formula_cell "p")] ();
    post = resource_set ~linear:[(new_anon_hyp (), formula_cell "p")] (); }

(* The environment containing the contracts of builtin functions. *)
let builtin_env = resource_set ~fun_contracts:(
  (* TODO: do something more readable than all the adds? e.g. Var_map.from_list [...]? *)
  Var_map.add "__new" (["init"],
    { pre = empty_resource_set;
      post = resource_set ~linear:[(new_anon_hyp (), formula_cell var_result)] () }) @@
  Var_map.add "__get" (["p"],
    push_read_only_fun_contract_res (None, formula_cell "p") empty_fun_contract) @@
  Var_map.add "__set" (["p"; "x"], set_fun_contract) @@
  Var_map.add "__add" (["x1"; "x2"], empty_fun_contract) @@
  Var_map.add "__sub" (["x1"; "x2"], empty_fun_contract) @@
  Var_map.add "__mul" (["x1"; "x2"], empty_fun_contract) @@
  Var_map.add "__array_access" (["tab"; "i"], empty_fun_contract) @@
  Var_map.add "__add_inplace" (["p"; "x"], set_fun_contract) @@
  Var_map.add "__sub_inplace" (["p"; "x"], set_fun_contract) @@
  Var_map.add "__mul_inplace" (["p"; "x"], set_fun_contract) @@
  (* TODO: Remove next 2 *)
  (*Var_map.add "__matrix2_get" (["p"; "m"; "n"; "i"; "j"],
    push_read_only_fun_contract_res (None, formula_matrix2 "p" (trm_var "m") (trm_var "n")) empty_fun_contract) @@
  Var_map.add "__matrix2_set" (["p"; "m"; "n"; "i"; "j"; "v"],
    { pre = resource_set ~linear:[new_anon_hyp (), formula_matrix2 "p" (trm_var "m") (trm_var "n")] ();
      post = resource_set ~linear:[new_anon_hyp (), formula_matrix2 "p" (trm_var "m") (trm_var "n")] (); }) @@*)
  Var_map.add "MINDEX2" (["m"; "n"; "i"; "j"], empty_fun_contract) @@
  Var_map.add "MINDEX3" (["sz1"; "sz2"; "sz3"; "i"; "j"; "k"], empty_fun_contract) @@
  Var_map.add "MINDEX4" (["sz1"; "sz2"; "sz3"; "sz4"; "i"; "j"; "k"; "l"], empty_fun_contract) @@
  Var_map.empty) ()

(* A formula that may instantiate contract variables with
   hypotheses from the calling context. *)
type formula_inst = formula

let inst_hyp (h: hyp): formula_inst =
  trm_make (Trm_hyp h)

let inst_hyp_inv (f: formula_inst) =
  match f.desc with
  | Trm_hyp h -> Some h
  | _ -> None

let inst_split_read_only ~(new_frac: var) (h: hyp) : formula_inst =
  trm_apps (trm_var "SplitRO") [trm_var new_frac; inst_hyp h]

let inst_split_read_only_inv (f: formula_inst): (var * hyp) option =
  let open Tools.OptionMonad in
  match trm_apps_inv f with
  | Some (fn, [frac; hyp]) ->
    begin match trm_var_inv fn with
    | Some "SplitRO" ->
      let* frac = trm_var_inv frac in
      let* hyp = inst_hyp_inv hyp in
      Some (frac, hyp)
    | _ -> None
    end
  | _ -> None


let rec unify_var (xe: var) (t: trm) (evar_ctx: unification_ctx) : unification_ctx option =
  let open Tools.OptionMonad in
  match Var_map.find_opt xe evar_ctx with
  | None ->
    (* [xe] cannot be substituted, it must be equal to [t]. *)
    let* x = trm_var_inv t in
    if x = xe then Some evar_ctx else None
  | Some None ->
    (* [xe] can be substituted, do it. *)
    Some (Var_map.add xe (Some t) evar_ctx)
  | Some (Some t_evar) ->
    (* [xe] was already substituted, its substitution must be equal to [t]. *)
    if are_same_trm t t_evar then Some evar_ctx else None

and are_same_trm (t1: trm) (t2: trm): bool =
  Option.is_some (unify_trm t1 t2 Var_map.empty)

and unify_trm (t: trm) (te: trm) (evar_ctx: unification_ctx) : unification_ctx option =
  let open Tools.OptionMonad in
  (* Pattern match on one component to get a warning if there is a missing one *)
  let check cond = if cond then Some evar_ctx else None in
  match te.desc with
  | Trm_var (_, xe) ->
    let xe = qvar_to_var xe in
    unify_var xe t evar_ctx
  | Trm_val ve ->
    let* v = trm_val_inv t in
    check (ve = v)
  | Trm_apps (fe, argse) ->
    let* f, args = trm_apps_inv t in
    let* evar_ctx = unify_trm f fe evar_ctx in
    List.fold_left2 (fun evar_ctx arg arge -> let* evar_ctx in unify_trm arg arge evar_ctx) (Some evar_ctx) args argse
  | Trm_fun (argse, _, bodye, _) ->
    let* args, _, body, _ = trm_fun_inv t in
    let* evar_ctx, masked_ctx =
      try
        Some (List.fold_left2 (fun (evar_ctx, masked) (arge, _) (arg, _) ->
            let masked_entry = Var_map.find_opt arge evar_ctx in
            let evar_ctx = Var_map.add arge (Some (trm_var arg)) evar_ctx in
            (evar_ctx, (arge, masked_entry) :: masked)
          ) (evar_ctx, []) argse args)
      with Invalid_argument _ -> None
    in
    let* evar_ctx = unify_trm body bodye evar_ctx in
    Some (List.fold_left (fun evar_ctx (arge, masked_entry) ->
        match masked_entry with
        | Some entry -> Var_map.add arge entry evar_ctx
        | None -> Var_map.remove arge evar_ctx
      ) evar_ctx masked_ctx)
  | _ -> failwith (sprintf "unify_trm: unhandled constructor %s" (AstC_to_c.ast_to_string t)) (* TODO: Implement the rest of constructors *)

(* TODO: is this different from trm_free_vars ? *)
let trm_used_vars (t: trm): Var_set.t =
  let vars = ref Var_set.empty in
  let rec aux t = match trm_var_inv t with
  | Some x -> vars := Var_set.add x !vars
  | _ -> trm_iter aux t
  in
  aux t;
  !vars

(* Combinator to apply a context dependant transformation on a trm.
   [f ctx t] returns the modified trm along with the context that should be
   used for the continuation of t (if it has any).
   This combinator allows one to get a view with continuations while the AST
   stores sequences. *)
let trm_map_with_ctx (f: 'ctx -> trm -> 'ctx * trm) (ctx: 'ctx) (t: trm): trm =
  let annot = t.annot in
  let loc = t.loc in

  let ret nochange t' =
    if nochange then t else t' in

  (* Sequence-like constructors must propagate context between their children.
     The rest of the constructors just follow the hierarchy. *)
  match t.desc with
  | Trm_seq tl ->
    let ctx = ref ctx in
    let tl' =
      Mlist.map (fun t ->
          let new_ctx, t' = f !ctx t in
          ctx := new_ctx;
          t'
        ) tl
    in
    ret (Mlist.for_all2 (==) tl tl')
      (trm_seq ~annot ?loc tl')
  | Trm_for_c (init, cond, step, body, invariant) ->
    let ctx, init' = f ctx init in
    let _, cond' = f ctx cond in
    let _, step' = f ctx step in
    let _, body' = f ctx body in
    ret (init' == init && cond' == cond && step' == step && body' == body)
      (trm_for_c ~annot ?loc ?invariant init' cond' step' body')
  | _ ->
    trm_map (fun ti -> let _, ti' = f ctx ti in ti') t

let trm_map_vars (map_binder: 'ctx -> var -> 'ctx * var) (map_var: 'ctx -> var -> trm) (ctx: 'ctx) (t: trm): trm =
  let annot = t.annot in
  let loc = t.loc in
  let ret nochange t' =
    if nochange then t else t' in

  let rec f_map ctx t: 'ctx * trm = match t.desc with
  | Trm_var (_, x) ->
    let x = qvar_to_var x in
    (ctx, map_var ctx x)

  | Trm_let (var_kind, (var, typ), body, bound_resources) ->
    let _, body' = f_map ctx body in
    let cont_ctx, var' = map_binder ctx var in
    let t = ret (body == body')
      (trm_let ~annot ?loc ?bound_resources var_kind (var', typ) body')
    in
    (cont_ctx, t)

  | Trm_let_fun (fn, res, args, body, contract) ->
    let body_ctx, args' = List.fold_left_map (fun ctx (arg, typ) -> let ctx, arg' = map_binder ctx arg in (ctx, (arg', typ))) ctx args in
    let _, body' = f_map body_ctx body in
    let cont_ctx, fn' = map_binder ctx (qvar_to_var fn) in
    let t' = ret (body' == body)
      (trm_let_fun ~annot ?loc ?contract fn' res args' body')
    in
    (* TODO: Proper function type here *)
    (cont_ctx, t')

  | Trm_for ((index, start, dir, stop, step, is_par), body, contract) ->
    let loop_ctx, index' = map_binder ctx index in
    let step' = match step with
    | Post_inc | Post_dec | Pre_inc | Pre_dec -> step
    | Step sp -> Step (snd (f_map loop_ctx sp))
    in
    let _, start' = f_map loop_ctx start in
    let _, stop' = f_map loop_ctx stop in
    let _, body' = f_map loop_ctx body in
    let t' = ret (step' == step && start' == start && stop' == stop && body' == body)
      (trm_for ~annot ?loc (index', start', dir, stop', step', is_par) body')
    in
    (ctx, t')

  | Trm_fun (args, ret, body, contract) ->
    let body_ctx, args' = List.fold_left_map (fun ctx (arg, typ) -> let ctx, arg' = map_binder ctx arg in (ctx, (arg', typ))) ctx args in
    let _, body' = f_map body_ctx body in
    let t' = trm_fun ~annot ?loc ?contract args' ret body' in
    (* TODO: Proper function type here *)
    (ctx, t')

  | _ -> (ctx, trm_map_with_ctx f_map ctx t)
  in
  snd (f_map ctx t)

(* TODO: Make a better naming with numbers later *)
let rec gen_var_name forbidden_names seed =
  if Var_set.mem seed forbidden_names then
    gen_var_name forbidden_names (seed ^ "'")
  else
    seed

(* FIXME: describe what this is about *)
let trm_subst_map_binder (forbidden_binders, subst_map) binder =
  if Var_set.mem binder forbidden_binders then
    let new_binder = gen_var_name forbidden_binders binder in
    let forbidden_binders = Var_set.add new_binder forbidden_binders in
    let subst_map = Var_map.add binder (trm_var new_binder) subst_map in
    ((forbidden_binders, subst_map), new_binder)
  else
    let forbidden_binders = Var_set.add binder forbidden_binders in
    let subst_map = Var_map.add binder (trm_var binder) subst_map in
    ((forbidden_binders, subst_map), binder)

let trm_subst_map_var (_, subst_map) var =
  match Var_map.find_opt var subst_map with
  | Some t -> t
  | None -> trm_var var

(* LATER: preserve shadowing *)
let trm_subst subst_map forbidden_binders t =
  trm_map_vars trm_subst_map_binder trm_subst_map_var (forbidden_binders, subst_map) t

(* FIXME: what is this doing? seems unused. *)
let rename_avoiding forbidden_binders t =
  trm_subst Var_map.empty forbidden_binders t

let subst_in_resources ?(forbidden_binders = Var_set.empty) (subst_map: tmap) (res: resource_set): tmap * resource_set =
  let subst_var_in_resource_list =
    List.fold_left_map (fun subst_ctx (h, t) ->
        let (forbidden_binders, subst_map) as subst_ctx, h =
          let subst_ctx, x = trm_subst_map_binder subst_ctx h.name in
          (subst_ctx, { h with name = x })
        in
        let t = trm_subst subst_map forbidden_binders t in
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

(* TODO: Use a real trm_fold later to avoid reconstructing trm *)
let trm_free_vars ?(bound_vars = Var_set.empty) (t: trm): Var_set.t =
  let fv = ref Var_set.empty in
  let _ = trm_map_vars (fun bound_set binder -> (Var_set.add binder bound_set, binder))
    (fun bound_set var ->
      (if Var_set.mem var bound_set then () else fv := Var_set.add var !fv); trm_var var)
    bound_vars t
  in
  !fv

let fun_contract_free_vars (contract: fun_contract): Var_set.t =
  let fold_res_list bound_vars fv res =
    List.fold_left (fun (bound_vars, fv) (h, formula) ->
      let bound_vars = Var_set.add h.name bound_vars in
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
  let subst_ctx = Var_map.mapi (fun var subst -> match subst with Some t -> t | None -> trm_var ("?" ^ var)) evar_ctx in
  let formula = trm_subst subst_ctx Var_set.empty formula in
  raise (Resource_not_found ((name, formula), inside))

(* Unify the given resource_item with one of the resources in the pure resource set.
   Also add a binding from x to the found resource in evar_ctx.
   If it fails raise a Resource_not_found exception. *)
let unify_pure ((x, formula): resource_item) (res: pure_resource_set) (evar_ctx: unification_ctx): unification_ctx =
  (* Add flag to disallow pure instantiation *)
  let exception Found of unification_ctx in
  let find_formula formula (hyp_candidate, formula_candidate) =
    match unify_trm formula_candidate formula evar_ctx with
    | Some evar_ctx -> raise (Found (Var_map.add x.name (Some (trm_var hyp_candidate.name)) evar_ctx))
    | None -> ()
  in
  try
    (* TODO: would a fold / recursion be easier to read? *)
    List.iter (find_formula formula) res;
    raise_resource_not_found (x, formula) evar_ctx res
  with Found evar_ctx -> evar_ctx

let rec unify_and_remove_linear ((x, formula): resource_item) (res: linear_resource_set)
  (evar_ctx: unification_ctx): used_resource_item * linear_resource_set * unification_ctx =
  (* LATER: Improve the structure of the linear_resource_set to make this
     function faster on most frequent cases *)
  let aux res = unify_and_remove_linear (x, formula) res evar_ctx in
  match res with
  (* TODO: Resource_not_found ? *)
  | [] -> raise Not_found
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
  (* TODO: Resource_not_found ? *)
  | [] -> raise Not_found
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

(* FIXME: explain relationship to unify_and_remove_linear *)
let subtract_linear_resource_item ~(split_frac: bool) ((x, formula): resource_item) (res: linear_resource_set)
  (evar_ctx: unification_ctx): used_resource_item * linear_resource_set * unification_ctx =
  try match formula_read_only_inv formula with
    | Some { frac; formula = ro_formula } when split_frac ->
      begin match trm_apps_inv frac with
      | Some (maybe_full, [frac]) when trm_var_inv maybe_full = Some "_Full" ->
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

let filter_evar_candidates (res: resource_set): resource_set * var list =
  (* TODO: maybe check free vars inside function contracts? *)
  (* This function completely forgets about ghost variable shadowing *)
  (* LATER: Un tri topologique serait un peu plus robuste *)
  let combine_used_vars used_vars (_, formula) =
    Var_set.union used_vars (trm_free_vars formula)
  in
  let used_vars = List.fold_left combine_used_vars Var_set.empty res.pure in
  let used_vars = List.fold_left combine_used_vars used_vars res.linear in
  let evar_candidates = ref [] in
  let pure = List.filter (fun (h, _) ->
      if Var_set.mem h.name used_vars then
        (evar_candidates := h.name :: !evar_candidates; false)
      else true) res.pure
  in
  ({ res with pure }, !evar_candidates)

exception Spec_not_found of var
exception NotConsumedResources of linear_resource_set
exception ImpureFunctionArgument of exn

(* [resource_impl_leftovers]: checks that [res_from] ==> [res_to] * [H] in
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
let rec resource_impl_leftovers ~(split_frac: bool) (res_from: resource_set) ?(subst_ctx: tmap = Var_map.empty) (res_to: resource_set) : tmap * used_resource_set * linear_resource_set =
  let filtered_res_to, evar_candidates = filter_evar_candidates res_to in
  let evar_ctx = List.fold_left (fun evar_ctx x -> Var_map.add x None evar_ctx)
      (Var_map.map (fun x -> Some x) subst_ctx) evar_candidates
  in

  let used_linear, leftover_linear, evar_ctx = subtract_linear_resource ~split_frac res_from.linear res_to.linear evar_ctx in
  let evar_ctx = List.fold_left (fun evar_ctx res_item ->
      unify_pure res_item res_from.pure evar_ctx) evar_ctx filtered_res_to.pure
  in

  (* All unifications should be done at this point. There is a bug if it's not the case. *)
  let subst_ctx = Var_map.map (function
      | Some t -> t
      | None -> failwith "failed unification") evar_ctx
  in

  let used_pure = List.map (fun (hyp, formula) ->
      { hyp_to_inst = hyp; inst_by = Var_map.find hyp.name subst_ctx; used_formula = trm_subst subst_ctx Var_set.empty formula }
    ) res_to.pure in

  ignore (Var_map.merge
            (fun fn_name spec_from spec_to ->
              match spec_from, spec_to with
              | _, None -> None (* We can drop extra specifications *)
              | None, Some _ -> raise (Spec_not_found fn_name)
              | Some spec_from, Some spec_to ->
                if spec_from = spec_to
                  then None
                  else failwith "resource_impl_leftovers: Unimplemented complex contract implications"
            )
            res_from.fun_contracts res_to.fun_contracts);

  (subst_ctx, { used_pure; used_linear }, leftover_linear)

(* [assert_resource_impl]: checks that [res_from] ==> [res_to] *)
and assert_resource_impl (res_from: resource_set) (res_to: resource_set) : used_resource_set =
  let _, used_res, leftovers = resource_impl_leftovers ~split_frac:false res_from res_to in
  if leftovers <> [] then raise (NotConsumedResources leftovers);
  used_res


let compute_produced_resources (subst_ctx: tmap) (contract_res: resource_set) : produced_resource_set =
  let forbidden_binders = Var_map.fold (fun _ formula acc -> Var_set.union acc (trm_free_vars formula)) subst_ctx Var_set.empty in
  let compute_produced_resources_list =
    List.fold_left_map (fun subst_ctx (h, formula) ->
        let produced_hyp = new_anon_hyp () in
        let produced_formula = trm_subst subst_ctx forbidden_binders formula in
        let produced = { produced_hyp; produced_from = h; produced_formula } in
        let subst_ctx = Var_map.add h.name (trm_var produced_hyp.name) subst_ctx in
        (subst_ctx, produced)
      )
  in
  let subst_ctx, produced_pure = compute_produced_resources_list subst_ctx contract_res.pure in
  let _, produced_linear = compute_produced_resources_list subst_ctx contract_res.linear in
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
    | Trm_var (_, qx) -> trm_var (qvar_to_var qx)
    | t -> trm_make t
  in
  trm_map_with_terminal_unopt false (fun _ -> trm_keep_only_desc) t

(* [resource_merge_after_frame]:
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
      begin match trm_var_inv frac_atom with
      | Some atom when Hashtbl.mem ro_formulas (ro_formula, atom) ->
        Hashtbl.remove ro_formulas (ro_formula, atom); sub_frac
      (* DEBUG:
      | Some atom ->
        Printf.eprintf "Failed to find %s\n" atom;
        Printf.eprintf "%s\n" (AstC_to_c.ast_to_string ro_formula);
        Hashtbl.iter (fun (a, _) () -> Printf.eprintf "%b\n%s\n" (a = ro_formula) (Ast_to_text.ast_to_string a)) ro_formulas;
        trm_sub sub_frac frac_atom*)
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
        let ro_formula = trm_keep_only_desc ro_formula in
        begin match trm_var_inv frac with
        | Some frac when Hashtbl.mem ro_formulas (ro_formula, frac) ->
          Hashtbl.remove ro_formulas (ro_formula, frac);
          (h, formula) :: acc
        | Some frac -> acc (* Consumed ro_formula *)
        | None -> (h, formula) :: acc
        end
      | None -> (h, formula) :: acc) frame res_after.linear in
  assert (Hashtbl.length ro_formulas = 0);
  { res_after with linear }

(* [bind_new_resources]: Add new pure resources to the old ones and replace linear resources *)
let bind_new_resources ~(old_res: resource_set) ~(new_res: resource_set): resource_set =
  { pure = new_res.pure @ old_res.pure;
    linear = new_res.linear;
    fun_contracts = Var_map.union (fun _ new_c _ -> Some new_c) new_res.fun_contracts old_res.fun_contracts }

let resource_names (res: resource_set) : Var_set.t =
  let res_list_names (res: resource_item list) =
    List.fold_left (fun avoid_names (h, _) ->
            Var_set.add h.name avoid_names) Var_set.empty res
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

let unop_to_var (u: unary_op): var =
  match u with
  | Unop_get -> "__get"
  | _ -> raise Unimplemented

let binop_to_var (u: binary_op): var =
  match u with
  | Binop_add -> "__add"
  | Binop_sub -> "__sub"
  | Binop_mul -> "__mul"
  | Binop_array_access -> "__array_access"
  | Binop_set -> "__set"
  | _ -> raise Unimplemented

let prim_to_var (p: prim): var =
  match p with
  | Prim_unop u -> unop_to_var u
  | Prim_binop b -> binop_to_var b
  | Prim_compound_assgn_op b -> binop_to_var b ^ "_inplace"
  | Prim_new _ -> "__new"
  | _ -> raise Unimplemented

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
    Some (Printf.sprintf "No specification for function %s" fn)
  | NotConsumedResources res ->
    Some (Printf.sprintf "Resources not consumed after the end of the block: '%s'" (Ast_fromto_AstC.ctx_resource_list_to_string res))
  | ImpureFunctionArgument err ->
    Some (Printf.sprintf "Function argument subexpression resource preservation check failed: %s" (Printexc.to_string err))
  | ResourceError (loc, ResourceComputation, err) ->
    Some (Printf.sprintf "%s: Resource computation error: %s" (loc_to_string loc) (Printexc.to_string err));
  | ResourceError (loc, ResourceCheck, err) ->
    Some (Printf.sprintf "%s: Resource check error: %s" (loc_to_string loc) (Printexc.to_string err))
  | _ -> None)

(* LATER: Extensible list of applications that can be translated into formula *)
let rec formula_of_trm (t: trm): formula option =
  (* FIXME: does [t] change or is this just checking whether trm_is_formula? *)
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
          | Some "MINDEX2"
          | Some "MINDEX3"
          | Some "MINDEX4"
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

(* FIXME: name is confusing because [compute_resources] already updates [t.ctx.ctx_resources_usage].
   Rename to [compute_usage_map_and_resources]? *)
let rec compute_resources_and_update_usage ?(expected_res: resource_spec) (res: resource_spec) (current_usage: resource_usage_map option) (t: trm): resource_usage_map option * resource_spec =
  let child_usage, res = compute_resources ?expected_res res t in
  let usage_map = update_usage_map_opt ~current_usage ~child_usage in
  (usage_map, res)

(* TODO: better name? *)
and compute_resources ?(expected_res: resource_spec) (res: resource_spec) (t: trm): resource_usage_map option * resource_spec =
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
      let name = qvar_to_var name in
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
          compute_resources_and_update_usage res usage_map inst)
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
        let _, _, linear = resource_impl_leftovers ~split_frac:false res res_to_free in
        { res with linear }) res
      in
      usage_map, res

    | Trm_let (_, (var, typ), body, spec) ->
      begin match spec with
      | Some bound_res ->
        (* FIXME: This breaks usage_map because it allows renaming without using the renamed hypothesis *)
        let expected_res = rename_var_in_resources var var_result bound_res in
        let usage_map, _ = compute_resources_and_update_usage ~expected_res (Some res) (Some usage_map) body in
        (* Use the bound_res contract but keep res existing pure facts *)
        usage_map, Some (bind_new_resources ~old_res:res ~new_res:bound_res)
      | None ->
        let usage_map, res_after = compute_resources_and_update_usage (Some res) (Some usage_map) body in
        usage_map, Option.map (fun res_after -> rename_var_in_resources var_result var res_after) res_after
      end

    | Trm_apps (fn, effective_args) ->
      let fn = trm_inv ~error:"Calling an anonymous function that is not bound to a variable is unsupported" trm_fun_var_inv fn in
      begin match Var_map.find_opt fn res.fun_contracts with
      | Some (contract_args, contract) ->
        (* TODO: Cast into readonly: better error message when a resource exists in RO only but asking for RW *)
        (* FIXME: I don't understand what is happening with this read only magic. *)
        let read_only_res = cast_into_read_only res in
        let contract_fv = fun_contract_free_vars contract in
        let subst_ctx, usage_map = try
          List.fold_left2 (fun (subst_map, usage_map) contract_arg effective_arg ->
            (* Give resources as read only and check that they are still there after the argument evaluation *)
            (* LATER: Collect pure facts of arguments:
               f(3+i), f(g(a)) where g returns a + a, f(g(a)) where g ensures res mod a = 0 *)
            let usage_map, post_arg_res = compute_resources_and_update_usage (Some read_only_res) usage_map effective_arg in
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
                | None -> fail effective_arg.loc (Printf.sprintf "Could not make a formula out of term '%s', required because of instantiation of %s contract" (AstC_to_c.ast_to_string effective_arg) fn)
              in
              Var_map.add contract_arg arg_formula subst_map, usage_map
            end else
              subst_map, usage_map
          ) (Var_map.empty, Some usage_map) contract_args effective_args;
        with Invalid_argument _ ->
          failwith (Printf.sprintf "Mismatching number of arguments for %s" fn)
        in

        let subst_ctx, res_used, res_frame = resource_impl_leftovers ~split_frac:true ~subst_ctx res contract.pre in

        let usage_map = Option.map (fun usage_map -> add_used_set_to_usage_map res_used usage_map) usage_map in

        let res_produced = compute_produced_resources subst_ctx contract.post in
        t.ctx.ctx_resources_contract_invoc <- Some {
            contract_frame = res_frame;
            contract_inst = res_used;
            contract_produced = res_produced };

        usage_map, Some (bind_new_resources ~old_res:res ~new_res:(resource_merge_after_frame res_produced res_frame))

      | None when fn = "__admitted" -> None, None
      | None -> raise (Spec_not_found fn)
      end

    | Trm_for (range, body, None) ->
      (* If no spec is given, put all the resources in the invariant (best effort) *)
      (* TODO: Still try to be clever about Group with a corresponding range *)
      let expected_res = resource_set ~linear:res.linear () in
      let usage_map, _ = compute_resources_and_update_usage ~expected_res (Some res) (Some usage_map) body in
      usage_map, Some res

    | Trm_for ((index, tstart, _, tend, step, _) as range, body, Some contract) ->
      (* Compute resources outside the loop *)
      let invariant_before = subst_var_in_resources index tstart contract.invariant in
      let before_loop_res = res_union invariant_before (res_group_range range contract.iter_contract.pre) in
      let before_loop_res = { before_loop_res with pure = contract.loop_ghosts @ before_loop_res.pure } in
      let ghost_subst_ctx, res_used, res_frame = resource_impl_leftovers ~split_frac:true res before_loop_res in

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
    | e -> raise (ResourceError (t.loc, ResourceComputation, e))
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

let ctx_copy (ctx: ctx): ctx = { ctx with ctx_types = ctx.ctx_types }

let rec trm_deep_copy (t: trm) : trm =
  let t = trm_map_with_terminal_unopt false (fun _ ti -> trm_deep_copy ti) t in
  t.ctx <- ctx_copy unknown_ctx; (* LATER *)
  t

let trm_recompute_resources (init_ctx: resource_set) (t: trm): trm =
  let t = trm_deep_copy t in
  ignore (compute_resources (Some init_ctx) t);
  t
