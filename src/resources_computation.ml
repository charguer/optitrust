open Ast
open Resources_contract

type pure_resource_set = resource_item list
type linear_resource_set = resource_item list

let var_result = "_Res"
let trm_result: formula = trm_var var_result

type evar = trm option
type unification_ctx = evar varmap

let set_fun_contract =
  { pre = resource_set ~linear:[(new_anon_hyp (), formula_cell "p")] ();
    post = resource_set ~linear:[(new_anon_hyp (), formula_cell "p")] (); }

let builtin_env = resource_set ~fun_contracts:(
  Var_map.add "__new" (["init"],
    { pre = empty_resource_set;
      post = resource_set ~linear:[(new_anon_hyp (), formula_cell var_result)] () }) @@
  Var_map.add "__get" (["p"],
    push_read_only_fun_contract_res (None, formula_cell "p") empty_fun_contract) @@
  Var_map.add "__set" (["p"; "x"], set_fun_contract) @@
  Var_map.add "__add" (["x1"; "x2"], empty_fun_contract) @@
  Var_map.add "__sub" (["x1"; "x2"], empty_fun_contract) @@
  Var_map.add "__mul" (["x1"; "x2"], empty_fun_contract) @@
  Var_map.add "__add_inplace" (["p"; "x"], set_fun_contract) @@
  Var_map.add "__sub_inplace" (["p"; "x"], set_fun_contract) @@
  Var_map.add "__mul_inplace" (["p"; "x"], set_fun_contract) @@
  Var_map.add "__matrix2_get" (["p"; "m"; "n"; "i"; "j"],
    push_read_only_fun_contract_res (None, formula_matrix2 "p" (trm_var "m") (trm_var "n")) empty_fun_contract) @@
  Var_map.add "__matrix2_set" (["p"; "m"; "n"; "i"; "j"; "v"],
    { pre = resource_set ~linear:[new_anon_hyp (), formula_matrix2 "p" (trm_var "m") (trm_var "n")] ();
      post = resource_set ~linear:[new_anon_hyp (), formula_matrix2 "p" (trm_var "m") (trm_var "n")] (); }) @@
  Var_map.empty) ()

let rec unify_var (xe: var) (t: trm) (evar_ctx: unification_ctx) : unification_ctx option =
  let open Tools.OptionMonad in
  match Var_map.find_opt xe evar_ctx with
  | None ->
    let* x = trm_var_inv t in
    if x = xe then Some evar_ctx else None
  | Some None ->
    Some (Var_map.add xe (Some t) evar_ctx)
  | Some (Some t_evar) ->
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
  | _ -> failwith "unify_trm: unhandled constructor" (* TODO: Implement the rest of constructors *)

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

  (* TODO: Missing Trm_fun, will be done when deduplicating trm_let_fun *)

  | _ -> (ctx, trm_map_with_ctx f_map ctx t)
  in
  snd (f_map ctx t)

(* TODO: Make a better naming with numbers later *)
let rec gen_var_name forbidden_names seed =
  if Var_set.mem seed forbidden_names then
    gen_var_name forbidden_names (seed ^ "'")
  else
    seed

let trm_subst_map_binder (forbidden_binders, subst_map) binder =
  if Var_set.mem binder forbidden_binders then
    let new_binder = gen_var_name forbidden_binders binder in
    let forbidden_binders = Var_set.add new_binder forbidden_binders in
    let subst_map = Var_map.add binder (trm_var new_binder) subst_map in
    ((forbidden_binders, subst_map), new_binder)
  else
    let forbidden_binders = Var_set.add binder forbidden_binders in
    ((forbidden_binders, subst_map), binder)

let trm_subst_map_var (_, subst_map) var =
  match Var_map.find_opt var subst_map with
  | Some t -> t
  | None -> trm_var var

(* LATER: preserve shadowing *)
let trm_subst subst_map forbidden_binders t =
  trm_map_vars trm_subst_map_binder trm_subst_map_var (forbidden_binders, subst_map) t

let rename_avoiding forbidden_binders t =
  trm_subst Var_map.empty forbidden_binders t

let subst_in_res ?(forbidden_binders = Var_set.empty) (subst_map: tmap) (res: resource_set): tmap * resource_set =
  let subst_var_in_resource_list =
    List.fold_left_map (fun subst_ctx (h, t) ->
        let (forbidden_binders, subst_map) as subst_ctx, h = match h.name with
          | Some x ->
            let subst_ctx, x = trm_subst_map_binder subst_ctx x in
            (subst_ctx, { h with name = Some x })
          | None -> (subst_ctx, h)
        in
        let t = trm_subst subst_map forbidden_binders t in
        (subst_ctx, (h, t))
      )
  in
  let subst_ctx, pure = subst_var_in_resource_list (forbidden_binders, subst_map) res.pure in
  let _, linear = subst_var_in_resource_list subst_ctx res.linear in
  (snd subst_ctx, { pure; linear;
    fun_contracts = res.fun_contracts (* TODO: subst here as well? *) })

let subst_var_in_res (x: var) (t: trm) (res: resource_set) : resource_set =
  snd (subst_in_res (Var_map.singleton x t) res)

let rename_var_in_res (x: var) (new_x: var) (res: resource_set) : resource_set =
  subst_var_in_res x (trm_var new_x) res

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
      let bound_vars = match h.name with
        | None -> bound_vars
        | Some x -> Var_set.add x bound_vars
      in
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

(* Unify the given resource_item with one of the resources in the pure resource set.
   If it fails raise a Resource_not_found exception. *)
let unify_pure ((x, formula): resource_item) (res: pure_resource_set) (evar_ctx: unification_ctx): unification_ctx =
  (* Add flag to disallow pure instantiation *)
  let exception Found of unification_ctx in
  let find_formula formula (_, formula_candidate) =
    match unify_trm formula_candidate formula evar_ctx with
    | Some evar_ctx -> raise (Found evar_ctx)
    | None -> ()
  in
  try
    List.iter (find_formula formula) res;
    raise (Resource_not_found ((x, formula), res))
  with Found evar_ctx -> evar_ctx

let rec unify_and_remove_linear (formula: formula) (res: linear_resource_set)
  (evar_ctx: unification_ctx): linear_resource_set * unification_ctx =
  (* LATER: Improve the structure of the linear_resource_set to make this
     function faster on most frequent cases *)
  let aux res = unify_and_remove_linear formula res evar_ctx in
  match res with
  | [] -> raise Not_found
  | (x, formula_candidate) as hyp_candidate :: res ->
    match unify_trm formula_candidate formula evar_ctx with
    | Some evar_ctx -> (res, evar_ctx)
    | None ->
      let res, evar_ctx = aux res in
      (hyp_candidate :: res, evar_ctx)

let rec unify_and_split_read_only ~(new_frac: var) (formula: formula) (res: linear_resource_set)
  (evar_ctx: unification_ctx): linear_resource_set * unification_ctx =
  (* LATER: Improve the structure of the linear_resource_set to make this
     function faster on most frequent cases *)
  let aux res = unify_and_split_read_only ~new_frac formula res evar_ctx in
  match res with
  | [] -> raise Not_found
  | (h, formula_candidate) as hyp_candidate :: res ->
    let cur_frac, formula_candidate = match formula_read_only_inv formula_candidate with
      | Some { frac; formula } -> frac, formula
      | None -> full_frac, formula_candidate
    in
    match unify_trm formula_candidate formula evar_ctx with
    | Some evar_ctx ->
      ((h, formula_read_only ~frac:(trm_sub cur_frac (trm_var new_frac)) formula_candidate) :: res, evar_ctx)
    | None ->
      let res, evar_ctx = aux res in
      (hyp_candidate :: res, evar_ctx)

let subtract_linear_res_item ~(split_frac: bool) ((x, formula): resource_item) (res: linear_resource_set)
  (evar_ctx: unification_ctx): linear_resource_set * unification_ctx =
  try match formula_read_only_inv formula with
    | Some { frac; formula = ro_formula } when split_frac ->
      begin match trm_var_inv frac with
      | Some frac_var ->
        begin match Var_map.find_opt frac_var evar_ctx with
        | Some None ->
          let new_frac, _ = new_frac () in (* LATER: store new generated frac ghosts *)
          let evar_ctx = Var_map.add frac_var (Some (trm_var new_frac)) evar_ctx in
          unify_and_split_read_only ~new_frac ro_formula res evar_ctx
        | _ ->
          unify_and_remove_linear formula res evar_ctx
        end
      | _ -> unify_and_remove_linear formula res evar_ctx
      end
    | _ -> unify_and_remove_linear formula res evar_ctx
  with Not_found ->
    raise (Resource_not_found ((x, formula), res))

(* [subtract_linear_res]: subtract [res_removed] from [res_from].
   Raise [Resource_not_found] if one resource is missing.
   Use the unification environment [evar_ctx] for all resources in [res_removed]
   potentially instantiating evars inside.
   If [split_frac] is true, always try to give a smaller fraction than what is
   inside [res_from] for evar fractions.
*)
let subtract_linear_res ~(split_frac: bool) (res_from: linear_resource_set) (res_removed: linear_resource_set)
  (evar_ctx: unification_ctx) : linear_resource_set * unification_ctx =
  List.fold_left (fun (res_from, evar_ctx) res_item ->
      subtract_linear_res_item ~split_frac res_item res_from evar_ctx) (res_from, evar_ctx) res_removed

let res_merge_after_frame (res_after: resource_set) (frame: linear_resource_set) =
  let ro_formulas = Hashtbl.create (List.length res_after.linear) in
  (* Try to recombine RO resources with those inside the frame *)
  List.iter (fun (_, formula) ->
      match formula_read_only_inv formula with
      | Some { frac; formula } ->
        begin match trm_var_inv frac with
        | Some frac_var -> Hashtbl.add ro_formulas (formula, frac_var) ()
        | None -> ()
        end
      | None -> ()
    ) res_after.linear;
  let rec reunite_fracs frac formula =
    match trm_binop_inv Binop_sub frac with
    | Some (sub_frac, frac_atom) ->
      let sub_frac = reunite_fracs sub_frac formula in
      begin match trm_var_inv frac_atom with
      | Some atom when Hashtbl.mem ro_formulas (formula, atom) ->
        Hashtbl.remove ro_formulas (formula, atom); sub_frac
      | _ -> trm_sub sub_frac frac_atom
      end
    | None -> frac
  in
  let frame = List.map (fun (x, formula) ->
      match formula_read_only_inv formula with
      | Some { frac; formula } ->
        let frac = reunite_fracs frac formula in
        if frac = full_frac
          then (x, formula)
          else (x, formula_read_only ~frac formula)
      | None -> (x, formula)
    ) frame in
  let linear = List.fold_left (fun acc (h, formula) ->
      match formula_read_only_inv formula with
      | Some { frac; formula = ro_formula } ->
        begin match trm_var_inv frac with
        | Some frac when Hashtbl.mem ro_formulas (formula, frac) ->
          Hashtbl.remove ro_formulas (formula, frac);
          (h, formula) :: acc
        | Some frac -> acc (* Consumed ro_formula *)
        | None -> (h, formula) :: acc
        end
      | None -> (h, formula) :: acc) frame res_after.linear in
  assert (Hashtbl.length ro_formulas = 0);
  { res_after with linear }

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
  let pure = List.filter (fun (h, _) -> match h.name with
      | Some x when Var_set.mem x used_vars ->
        evar_candidates := x :: !evar_candidates; false
      | _ -> true) res.pure
  in
  ({ res with pure }, !evar_candidates)

exception Spec_not_found of var
exception NotConsumedResources of linear_resource_set
exception ImpureFunctionArgument of exn

(* [res_impl_leftovers]: checks that [res_from] ==> [res_to] * [H] in
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
(* Complexifier le type de retour pour séparer RO frame *)
let rec res_impl_leftovers ~(split_frac: bool) (res_from: resource_set) ?(evar_ctx: unification_ctx option) (res_to: resource_set) : tmap * linear_resource_set =
  let evar_ctx, res_to = match evar_ctx with
    | Some evar_ctx -> (evar_ctx, res_to)
    | None ->
      let res_to, evar_candidates = filter_evar_candidates res_to in
      let evar_ctx = List.fold_left (fun evar_ctx x -> Var_map.add x None evar_ctx) Var_map.empty evar_candidates in
      (evar_ctx, res_to)
  in

  let leftover_linear, evar_ctx = subtract_linear_res ~split_frac res_from.linear res_to.linear evar_ctx in
  let evar_ctx = List.fold_left (fun evar_ctx res_item ->
      unify_pure res_item res_from.pure evar_ctx) evar_ctx res_to.pure
  in

  (* All unifications should be done at this point. There is a bug if it's not the case. *)
  let subst_ctx = Var_map.map (function
      | Some t -> t
      | None -> failwith "failed unification") evar_ctx
  in

  ignore (Var_map.merge
            (fun fn_name spec_from spec_to ->
              match spec_from, spec_to with
              | _, None -> None (* We can drop extra specifications *)
              | None, Some _ -> raise (Spec_not_found fn_name)
              | Some spec_from, Some spec_to ->
                if spec_from = spec_to
                  then None
                  else failwith "res_impl_leftovers: Unimplemented complex contract implications"
            )
            res_from.fun_contracts res_to.fun_contracts);

  (subst_ctx, leftover_linear)

(* [assert_res_impl]: checks that [res_from] ==> [res_to] *)
and assert_res_impl (res_from: resource_set) (res_to: resource_set) : unit =
  let _, leftovers = res_impl_leftovers ~split_frac:false res_from res_to in
  if leftovers <> [] then raise (NotConsumedResources leftovers)

and assert_res_impl_opt (res_from: resource_spec) (res_to: resource_spec) : unit =
  match res_from, res_to with
  | Some res_from, Some res_to -> assert_res_impl res_from res_to
  | _, _ -> () (* Skip checks if any of the two is undefined *)

(* [bind_new_resources]: Add new pure resources to the old ones and replace linear resources *)
let bind_new_resources ~(old_res: resource_set) ~(new_res: resource_set): resource_set =
  { pure = new_res.pure @ old_res.pure;
    linear = new_res.linear;
    fun_contracts = Var_map.union (fun _ new_c _ -> Some new_c) new_res.fun_contracts old_res.fun_contracts }

let resource_names (res: resource_set) =
  let res_list_names (res: resource_item list) =
    List.fold_left (fun avoid_names (h, _) ->
            match h.name with
            | Some x -> Var_set.add x avoid_names
            | None -> avoid_names) Var_set.empty res
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
  Printf.sprintf "[%s] {%s}" spure slin
  | None -> "UnspecifiedRes"

type resource_error_phase = ResourceComputation | ResourceCheck
exception ResourceError of location * resource_error_phase * exn

let _ = Printexc.register_printer (function
  | Resource_not_found (item, context) ->
    Some (Printf.sprintf "Resource %s not found in context %s" (Ast_fromto_AstC.named_formula_to_string item) (Ast_fromto_AstC.ctx_resource_list_to_string context))
  | Spec_not_found fn ->
    Some (Printf.sprintf "No specification for function %s" fn)
  | NotConsumedResources res ->
    Some (Printf.sprintf "Resources not consumed after the end of the block: %s" (Ast_fromto_AstC.ctx_resource_list_to_string res))
  | ImpureFunctionArgument err ->
    Some (Printf.sprintf "Impure function argument: %s" (Printexc.to_string err))
  | ResourceError (loc, ResourceComputation, err) ->
    Some (Printf.sprintf "%s: Resource computation error: %s" (loc_to_string loc) (Printexc.to_string err));
  | ResourceError (loc, ResourceCheck, err) ->
    Some (Printf.sprintf "%s: Resource check error: %s" (loc_to_string loc) (Printexc.to_string err))
  | _ -> None)

(* LATER: Extensible list of applications that can be translated into formula *)
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
          -> Some (trm_apps fn f_args)
      | _ -> None
    end
  | _ -> None

let matrix2_access_inv (t: trm): (trm * trm * trm * trm * trm) option =
  let open Tools.OptionMonad in
  let* tfn, targs = trm_apps_inv t in
  let* tmat, tindex = match trm_prim_inv tfn, targs with
    | Some (Prim_binop Binop_array_access), [tmat; tindex] -> Some (tmat, tindex)
    | _ -> None
  in
  let* tidxfn, tidxargs = trm_apps_inv tindex in
  match trm_var_inv tidxfn, tidxargs with
  | Some "MINDEX2", [tm; tn; ti; tj] -> Some (tmat, tm, tn, ti, tj)
  | _ -> None

(* Currenty array accesses are composed of weird functions calls that cannot
 * be given an easy spec to work with. For the resource computation we replace
 * there calls into more straigthforward array accesses.
 * LATER: Find a convenient way of encode array accesses such that both
 * transformations and resource computation agree on the interface. *)
let decode_array_access (fn: trm) (args: trm list): trm * trm list =
  match trm_prim_inv fn, args with
  | Some (Prim_unop Unop_get), [tptr] ->
    begin match matrix2_access_inv tptr with
    | Some (tmat, tm, tn, ti, tj) -> ((trm_var "__matrix2_get"), [tmat; tm; tn; ti; tj])
    | _ -> (fn, args)
    end
  | Some (Prim_binop Binop_set), [tptr; tval] ->
    begin match matrix2_access_inv tptr with
    | Some (tmat, tm, tn, ti, tj) -> ((trm_var "__matrix2_set"), [tmat; tm; tn; ti; tj; tval])
    | _ -> (fn, args)
    end
  | _ -> (fn, args)

(* TODO: better name? *)
let rec compute_inplace ?(expected_res: resource_spec) (res: resource_spec) (t: trm): resource_spec =
  let aux = compute_inplace in
  (*Printf.eprintf "With resources: %s\nComputing %s\n\n" (resources_to_string res) (AstC_to_c.ast_to_string t);*)
  t.ctx.ctx_resources_before <- res;
  let open Tools.OptionMonad in
  let res =
    let* res in
    try begin match t.desc with
    | Trm_val _ | Trm_var _ -> Some res (* TODO: Manage return values for pointers *)

    | Trm_let_fun (name, ret_type, args, body, contract) ->
      let name = qvar_to_var name in
      begin match contract with
      | Some contract ->
        let body_res = bind_new_resources ~old_res:res ~new_res:contract.pre in
        ignore (aux ~expected_res:contract.post (Some body_res) body);
        let args = List.map (fun (x, _) -> x) args in
        Some { res with fun_contracts = Var_map.add name (args, contract) res.fun_contracts }
      | None -> Some res
      end

    | Trm_seq instrs ->
      let instrs = Mlist.to_list instrs in
      let* res = List.fold_left aux (Some res) instrs in

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
      let _, linear = res_impl_leftovers ~split_frac:false res res_to_free in
      Some { res with linear }

    | Trm_let (_, (var, typ), body, spec) ->
      begin match spec with
      | Some bound_res ->
        let expected_res = rename_var_in_res var var_result bound_res in
        ignore (aux ~expected_res (Some res) body);
        (* Use the bound_res contract but keep res existing pure facts *)
        Some (bind_new_resources ~old_res:res ~new_res:bound_res)
      | None ->
        let* res_after = aux (Some res) body in
        Some (rename_var_in_res var_result var res_after)
      end

    | Trm_apps (fn, effective_args) ->
      let (fn, effective_args) = decode_array_access fn effective_args in

      let fn = trm_inv ~error:"Calling an anonymous function that is not bound to a variable is unsupported" trm_fun_var_inv fn in
      begin match Var_map.find_opt fn res.fun_contracts with
      | Some (contract_args, contract) ->
        let subst_map = ref Var_map.empty in
        let avoid_names = ref (resource_names res) in (* FIXME: This should also contain program variable names *)
        (* TODO: Cast into readonly: better error message when a resource exists in RO only but asking for RW *)
        let read_only_res = cast_into_read_only res in
        let contract_fv = fun_contract_free_vars contract in
        begin try
          List.iter2 (fun contract_arg effective_arg ->
            (* Give resources as read only and check that they are still there after the argument evaluation *)
            (* LATER: Collect pure facts of arguments:
               f(3+i), f(g(a)) where g returns a + a, f(g(a)) where g ensures res mod a = 0 *)
            begin match compute_inplace (Some read_only_res) effective_arg with
            | Some arg_post ->
              begin try assert_res_impl arg_post (resource_set ~linear:read_only_res.linear ())
              with e -> raise (ImpureFunctionArgument e)
              end
            | None -> ()
            end;

            if Var_set.mem contract_arg contract_fv then begin
              let arg_formula = match formula_of_trm effective_arg with
                | Some formula -> formula
                | None -> fail effective_arg.loc (Printf.sprintf "Could not make a formula out of term '%s', required because of instantiation of %s contract" (AstC_to_c.ast_to_string effective_arg) fn)
              in
              subst_map := Var_map.add contract_arg arg_formula !subst_map;
              avoid_names := Var_set.union (trm_free_vars effective_arg) !avoid_names
            end
          ) contract_args effective_args;
        with Invalid_argument _ ->
          failwith (Printf.sprintf "Mismatching number of arguments for %s" fn)
        end;
        let subst_ctx, effective_pre = subst_in_res ~forbidden_binders:!avoid_names !subst_map contract.pre in

        let effective_pre, evar_candidates = filter_evar_candidates effective_pre in
        let evar_ctx = List.fold_left (fun evar_ctx x -> Var_map.add x None evar_ctx) Var_map.empty evar_candidates in

        let ghost_subst_ctx, res_frame = res_impl_leftovers ~split_frac:true res ~evar_ctx effective_pre in
        t.ctx.ctx_resources_frame <- Some res_frame;
        let ghost_subst_ctx, effective_pre = subst_in_res ~forbidden_binders:!avoid_names ghost_subst_ctx effective_pre in
        t.ctx.ctx_resources_used <- Some effective_pre;

        let _, inst_post = subst_in_res ~forbidden_binders:!avoid_names subst_ctx contract.post in
        let _, inst_post = subst_in_res ~forbidden_binders:!avoid_names ghost_subst_ctx inst_post in
        t.ctx.ctx_resources_produced <- Some inst_post;
        Some (bind_new_resources ~old_res:res ~new_res:(res_merge_after_frame inst_post res_frame))

      | None when fn = "__admitted" -> None
      | None -> raise (Spec_not_found fn)
      end

    | Trm_for (range, body, None) ->
      (* If no spec is given, put all the resources in the invariant (best effort) *)
      (* TODO: Still try to be clever about Group with a corresponding range *)
      let expected_res = resource_set ~linear:res.linear () in
      ignore (aux ~expected_res (Some res) body);
      Some res

    | Trm_for ((index, tstart, _, tend, _, _) as range, body, Some contract) ->
      let invariant_before = subst_var_in_res index tstart contract.invariant in
      let before_loop_res = res_union invariant_before (res_group_range range contract.iter_contract.pre) in
      let before_loop_res = { before_loop_res with pure = contract.loop_ghosts @ before_loop_res.pure } in
      let ghost_subst_ctx, res_frame = res_impl_leftovers ~split_frac:true res before_loop_res in

      let loop_body_pre = res_union contract.invariant contract.iter_contract.pre in
      let loop_body_pre = { loop_body_pre with fun_contracts = res.fun_contracts } in
      let loop_body_post = res_union contract.invariant contract.iter_contract.post in
      ignore (aux ~expected_res:loop_body_post (Some loop_body_pre) body);

      let _, invariant_after = subst_in_res (Var_map.add index tend ghost_subst_ctx) contract.invariant in
      let after_loop_res = res_union invariant_after (res_group_range range contract.iter_contract.post) in
      Some (bind_new_resources ~old_res:res ~new_res:(res_merge_after_frame after_loop_res res_frame))

    | _ -> fail t.loc ("Resource_core.compute_inplace: not implemented for " ^ AstC_to_c.ast_to_string t)
    end with e when !Flags.resource_errors_as_warnings ->
      Printf.eprintf "%s: Resource computation warning: %s\n" (loc_to_string t.loc) (Printexc.to_string e);
      None
    | ResourceError (None, place, err) -> raise (ResourceError (t.loc, place, err))
    | ResourceError (Some _, _, _) as e -> raise e
    | e -> raise (ResourceError (t.loc, ResourceComputation, e))
  in

  (*Printf.eprintf "With resources: %s\nSaving %s\n\n" (resources_to_string res) (AstC_to_c.ast_to_string t);*)
  t.ctx.ctx_resources_after <- res;
  match res, expected_res with
    | Some res, Some expected_res ->
      begin try assert_res_impl res expected_res
      with e when !Flags.resource_errors_as_warnings ->
        Printf.eprintf "%s: Resource check warning: %s\n" (loc_to_string t.loc) (Printexc.to_string e);
      | ResourceError _ as e -> raise e
      | e -> raise (ResourceError (t.loc, ResourceCheck, e))
      end;
      Some expected_res
    | _, None -> res
    | None, _ -> expected_res

let ctx_copy (ctx: ctx): ctx = { ctx with ctx_types = ctx.ctx_types }

let rec trm_deep_copy (t: trm) : trm =
  let t = trm_map_with_terminal_unopt false (fun _ ti -> trm_deep_copy ti) t in
  t.ctx <- ctx_copy unknown_ctx; (* LATER *)
  t

let trm_recompute_resources (init_ctx: resource_set) (t: trm): trm =
  let t = trm_deep_copy t in
  ignore (compute_inplace (Some init_ctx) t);
  t
