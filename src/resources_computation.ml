open Ast
open Resources_contract

type resource_item = (hyp option * formula)
type pure_resource_set = resource_item list
type linear_resource_set = resource_item list

let var_result = "_Res"
let trm_result: formula = trm_var var_result

type evar = trm option
type unification_ctx = evar varmap

let set_fun_contract =
  { pre = resource_set ~linear:[(None, formula_cell "p")] ();
    post = resource_set ~linear:[(None, formula_cell "p")] (); }

let builtin_env = resource_set ~fun_contracts:(
  Var_map.add "__new" (["init"],
    { pre = empty_resource_set;
      post = resource_set ~linear:[(None, formula_cell var_result)] () }) @@
  Var_map.add "__get" (["p"],
    { pre = push_read_only_res (None, formula_cell "p") empty_resource_set;
      post = empty_resource_set; }) @@
  Var_map.add "__set" (["p"; "x"], set_fun_contract) @@
  Var_map.add "__add" (["x1"; "x2"], empty_fun_contract) @@
  Var_map.add "__add_inplace" (["p"; "x"], set_fun_contract) @@
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
    List.fold_left_map (fun subst_ctx (x, t) ->
        let (forbidden_binders, subst_map) as subst_ctx, x = match x with
          | Some x ->
            let subst_ctx, x = trm_subst_map_binder subst_ctx x in
            (subst_ctx, Some x)
          | None -> (subst_ctx, None)
        in
        let t = trm_subst subst_map forbidden_binders t in
        (subst_ctx, (x, t))
      )
  in
  let subst_ctx, pure = subst_var_in_resource_list (forbidden_binders, subst_map) res.pure in
  let _, linear = subst_var_in_resource_list subst_ctx res.linear in
  (snd subst_ctx, { pure; linear;
    fun_contracts = res.fun_contracts (* TODO: subst here as well? *) })

let rename_var_in_res (x: var) (new_x: var) (res: resource_set) : resource_set =
  snd (subst_in_res (Var_map.singleton x (trm_var new_x)) res)

(* TODO: Use a real trm_fold later to avoid reconstructing trm *)
let trm_free_vars (t: trm): Var_set.t =
  let fv = ref Var_set.empty in
  let _ = trm_map_vars (fun bound_set binder -> (Var_set.add binder bound_set, binder))
    (fun bound_set var ->
      (if Var_set.mem var bound_set then () else fv := Var_set.add var !fv); trm_var var)
    Var_set.empty t
  in
  !fv

(* [Resource_not_found (item, res_list)]: exception raised when the resource
   [item] is not found inside the resource list [res_list] *)
exception Resource_not_found of resource_item * resource_item list

(* Unify the given resource_item with one of the resources in the pure resource set.
   If it fails raise a Resource_not_found exception.
   If it is a read only resource and leftover_linear is given, also try to
   unify the read only formula inside leftover_linear. *)
let unify_pure ((x, formula): resource_item) (res: pure_resource_set) ?(leftover_linear: linear_resource_set = []) (evar_ctx: unification_ctx): unification_ctx =
  (* Add flag to disallow pure instantiation *)
  let exception Found of unification_ctx in
  let find_formula formula (_, formula_candidate) =
    match unify_trm formula_candidate formula evar_ctx with
    | Some evar_ctx -> raise (Found evar_ctx)
    | None -> ()
  in
  try
    List.iter (find_formula formula) res;
    begin match trm_read_only_inv formula with
    | Some ro_formula -> List.iter (find_formula ro_formula) leftover_linear
    | None -> ()
    end;
    raise (Resource_not_found ((x, formula), res))
  with Found evar_ctx -> evar_ctx

let unify_and_remove_linear ((x, formula): resource_item) (res: linear_resource_set)
  (evar_ctx: unification_ctx): linear_resource_set * unification_ctx =
  (* LATER: Improve the structure of the linear_resource_set to make this
     function faster on most frequent cases *)
  let rec aux = function
  | [] -> raise Not_found
  | (x, formula_candidate) :: res ->
    match unify_trm formula_candidate formula evar_ctx with
    | Some evar_ctx -> (res, evar_ctx)
    | None ->
      let res, evar_ctx = aux res in
      ((x, formula_candidate) :: res, evar_ctx)
  in
  try aux res
  with Not_found ->
    raise (Resource_not_found ((x, formula), res))

(* [subtract_linear_res]: subtract [res_removed] from [res_from].
   Raise [Resource_not_found] if one resource is missing.
   If [subst_ctx] is given, apply the substitution environment to all resources in
   [res_removed] potentially instantiating evars inside.
*)
let subtract_linear_res (res_from: linear_resource_set) (res_removed: linear_resource_set)
  (evar_ctx: unification_ctx) : linear_resource_set * unification_ctx =
  List.fold_left (fun (res_from, evar_ctx) res_item ->
      unify_and_remove_linear res_item res_from evar_ctx) (res_from, evar_ctx) res_removed

let filter_evar_candidates (res: resource_set): resource_set * var list =
  (* TODO: maybe check free vars inside function contracts? *)
  (* This function completely forgets about ghost variable shadowing *)
  let combine_used_vars used_vars (_, formula) =
    Var_set.union used_vars (trm_free_vars formula)
  in
  let used_vars = List.fold_left combine_used_vars Var_set.empty res.pure in
  let used_vars = List.fold_left combine_used_vars used_vars res.linear in
  let evar_candidates = ref [] in
  let pure = List.filter (fun (x, _) -> match x with
      | Some x when Var_set.mem x used_vars ->
        evar_candidates := x :: !evar_candidates; false
      | _ -> true) res.pure
  in
  ({ res with pure }, !evar_candidates)

exception Spec_not_found of var
exception NotConsumedResources of linear_resource_set

(* [res_impl_leftovers]: checks that [res_from] ==> [res_to] * [H] in
   separation logic and return the leftover linear resources [H] along with
   the substitution context after instantiating ghost variables in [res_to]:
   effectively, this checks that all resources inside [res_to] can be built
   from resources inside [res_from] and returns the remaining linear resources
   after instantiation.
   If given, the substitution context [subst_ctx] is applied to [res_to] during
   the comparisons.
   Pure resources (ghosts) can be inferred using unification.

   TODO: Add unit tests for this specific function
*)
let rec res_impl_leftovers (res_from: resource_set) ?(subst_ctx: tmap = Var_map.empty) (res_to: resource_set) : tmap * linear_resource_set =
  (* LATER: Un tri topologique serait un peu plus robuste *)
  let res_to, evar_candidates = filter_evar_candidates res_to in
  let evar_ctx = Var_map.map (fun x -> Some x) subst_ctx in
  let evar_ctx = List.fold_left (fun evar_ctx x -> Var_map.add x None evar_ctx) evar_ctx evar_candidates in

  let leftover_linear, evar_ctx = subtract_linear_res res_from.linear res_to.linear evar_ctx in
  let evar_ctx = List.fold_left (fun evar_ctx res_item ->
      unify_pure res_item res_from.pure ~leftover_linear evar_ctx) evar_ctx res_to.pure
  in

  (* LATER: Display RO frames? *)

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
  let _, leftovers = res_impl_leftovers res_from res_to in
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
    List.fold_left (fun avoid_names (x, _) ->
            match x with
            | Some x -> Var_set.add x avoid_names
            | None -> avoid_names) Var_set.empty res
  in
  Var_set.union (res_list_names res.pure) (res_list_names res.linear)

exception Unimplemented

let unop_to_var (u: unary_op): var =
  match u with
  | Unop_get -> "__get"
  | _ -> raise Unimplemented

let binop_to_var (u: binary_op): var =
  match u with
  | Binop_add -> "__add"
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

let _ = Printexc.register_printer (function
  | Resource_not_found (item, context) ->
    Some (Printf.sprintf "Resource %s not found in context %s" (Ast_fromto_AstC.named_formula_to_string item) (Ast_fromto_AstC.ctx_resource_list_to_string context))
  | Spec_not_found fn ->
    Some (Printf.sprintf "No specification for function %s" fn)
  | NotConsumedResources res ->
    Some (Printf.sprintf "Resources not consumed after the end of the block: %s" (Ast_fromto_AstC.ctx_resource_list_to_string res))
  | _ -> None)

(* TODO: better name *)
let rec compute_inplace ?(expected_res: resource_spec) (res: resource_spec) (t: trm): resource_spec =
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
        ignore (compute_inplace ?expected_res:(Some contract.post) (Some body_res) body);
        let args = List.map (fun (x, _) -> x) args in
        Some { res with fun_contracts = Var_map.add name (args, contract) res.fun_contracts }
      | None -> Some res
      end

    | Trm_seq instrs ->
      (*let add_dummy_let t =*)
      (*  match t.desc with*)
      (*  | Trm_let _ | Trm_let_fun _ -> t*)
      (*  | _ -> trm_let Var_immutable ("__dummy", typ_auto ()) t*)
      (*in*)
      let instrs = Mlist.to_list instrs in
      (*let instrs = List.map add_dummy_let instrs in*)
      let* res = List.fold_left compute_inplace (Some res) instrs in

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
      let res_to_free = resource_set ~linear:(List.map (fun x -> (None, formula_cell x)) to_free) () in
      let _, linear = res_impl_leftovers res res_to_free in
      Some { res with linear }

    | Trm_let (_, (var, typ), body, spec) ->
      begin match spec with
      | Some bound_res ->
        let expected_res = rename_var_in_res var var_result bound_res in
        ignore (compute_inplace ~expected_res (Some res) body);
        (* Use the bound_res contract but keep res existing pure facts *)
        Some (bind_new_resources ~old_res:res ~new_res:bound_res)
      | None ->
        let* res_after = compute_inplace (Some res) body in
        Some (rename_var_in_res var_result var res_after)
      end

    | Trm_apps (fn, args) ->
      let fn = trm_inv ~error:"Calling an anonymous function that is not bound to a variable is unsupported" trm_fun_var_inv fn in
      begin match Var_map.find_opt fn res.fun_contracts with
      | Some (contract_args, { pre ; post }) ->
        (* TODO: Cast into readonly: attention à l'erreur générée *)
        (* f(3+i), f(g(a)) où g renvoie a + a, f(g(a)) où g ensures res mod a = 0 *)
        let subst_map = ref Var_map.empty in
        let avoid_names = ref (resource_names pre) in
        begin try
          List.iter2 (fun contract_arg effective_arg ->
            subst_map := Var_map.add contract_arg effective_arg !subst_map;
            avoid_names := Var_set.union (trm_free_vars effective_arg) !avoid_names
          ) contract_args args;
        with Invalid_argument _ ->
          failwith (Printf.sprintf "Mismatching number of arguments for %s" fn)
        end;
        let subst_ctx, effective_pre = subst_in_res ~forbidden_binders:!avoid_names !subst_map pre in
        let subst_ctx, res_frame = res_impl_leftovers res ~subst_ctx effective_pre in
        t.ctx.ctx_resources_frame <- Some res_frame;

        let _, inst_post = subst_in_res subst_ctx post in
        Some (bind_new_resources ~old_res:res ~new_res:{ inst_post with linear = inst_post.linear @ res_frame })

      | None when fn = "__admitted" -> None
      | None -> raise (Spec_not_found fn)
      end

    | _ -> failwith ("Resource_core.compute_inplace: not implemented for " ^ AstC_to_c.ast_to_string t)
    end with e when !Flags.resource_errors_as_warnings ->
      Printf.eprintf "%s: Resource computation warning: %s\n" (loc_to_string t.loc) (Printexc.to_string e);
      None
  in

  (*Printf.eprintf "With resources: %s\nSaving %s\n\n" (resources_to_string res) (AstC_to_c.ast_to_string t);*)
  t.ctx.ctx_resources_after <- res;
  match res, expected_res with
  | Some res, Some expected_res ->
    begin try assert_res_impl res expected_res
    with e when !Flags.resource_errors_as_warnings ->
      Printf.eprintf "%s: Resource check warning: %s\n" (loc_to_string t.loc) (Printexc.to_string e);
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
