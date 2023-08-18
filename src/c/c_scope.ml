open Ast
open Trm

let debug = true

(* FIXME: triggers on multiple function declarations/definitions. *)
exception InvalidVarId of string

(** Set of variables defined in the current surrounding sequence, and maps from variables to their unique ids. *)
type scope_ctx = Qualified_set.t * var_id Qualified_map.t

(* FIXME: code mostly duplicated from [trm_map_vars] *)
let rec map_vars
  (map_var: scope_ctx -> metadata -> var -> trm)
  (map_binder: scope_ctx -> var -> scope_ctx * var)
  (scope_ctx: scope_ctx) (t: trm): scope_ctx * trm =
  let aux = map_vars map_var map_binder in
  let annot = t.annot in
  let loc = t.loc in
  let typ = t.typ in
  let ctx = t.ctx in
  let ret nochange t' = if nochange then t else t' in
  match t.desc with
  | Trm_var (kind, x) ->
    (scope_ctx, map_var scope_ctx (annot, loc, typ, ctx, kind) x)

  | Trm_seq tl ->
    let sctx = ref (Qualified_set.empty, snd scope_ctx) in
    let tl' = Mlist.map (fun t ->
      let new_ctx, t' = aux !sctx t in
      sctx := new_ctx;
      t'
    ) tl in
    let t' = ret (Mlist.for_all2 (==) tl tl')
      (trm_seq ~annot ?loc ~ctx tl')
    in
    (scope_ctx, t')

  | Trm_let (var_kind, (var, typ), body, bound_resources) ->
    let _, body' = aux scope_ctx body in
    let cont_ctx, var' = map_binder scope_ctx var in
    let t' = ret (body == body' && var == var')
      (trm_let ~annot ?loc ~ctx ?bound_resources var_kind (var', typ) body')
    in
    (cont_ctx, t')

  | Trm_let_mult (vk, tvs, ts) ->
    (* CHECK: #var-id, is this correct? *)
    let ts' = List.map (fun t -> snd (aux scope_ctx t)) ts in
    let cont_ctx = ref scope_ctx in
    let tvs' = List.map (fun tv ->
      let var, typ = tv in
      let cont_ctx', var' = map_binder !cont_ctx var in
      cont_ctx := cont_ctx';
      if var == var' then tv else (var', typ)
    ) tvs in
    let t' = ret ((List.for_all2 (==) ts ts') && List.for_all2 (==) tvs tvs')
      (trm_let_mult ~annot ?loc ~ctx vk tvs' ts')
    in
    (!cont_ctx, t')

  | Trm_let_fun (fn, res, args, body, contract) ->
    let body_ctx, args' = List.fold_left_map (fun sctx (arg, typ) ->
      let sctx, arg' = map_binder sctx arg in
      (sctx, (arg', typ))
    ) scope_ctx args in
    let _, body' = aux body_ctx body in
    let cont_ctx, fn' = map_binder scope_ctx fn in
    let t' = ret (body' == body && args == args' && fn == fn')
      (trm_let_fun ~annot ?loc ~ctx ?contract fn' res args' body')
    in
    (cont_ctx, t')

  | Trm_for ((index, start, dir, stop, step, is_par), body, contract) ->
    let loop_ctx, index' = map_binder scope_ctx index in
    let step' = match step with
    | Post_inc | Post_dec | Pre_inc | Pre_dec -> step
    | Step sp -> Step (snd (aux loop_ctx sp))
    in
    let _, start' = aux loop_ctx start in
    let _, stop' = aux loop_ctx stop in
    let _, body' = aux loop_ctx body in
    let t' = ret (index' == index && step' == step && start' == start && stop' == stop && body' == body)
      (trm_for ~annot ?loc ~ctx (index', start', dir, stop', step', is_par) body')
    in
    (scope_ctx, t')

  | Trm_for_c (init, cond, step, body, invariant) ->
    let init_ctx, init' = aux scope_ctx init in
    let _, cond' = aux init_ctx cond in
    let _, step' = aux init_ctx step in
    let _, body' = aux init_ctx body in
    let t' = ret (init' == init && cond' == cond && step' == step && body' == body)
      (trm_for_c ~annot ?loc ~ctx ?invariant init' cond' step' body')
    in
    (scope_ctx, t')

  | Trm_fun (args, ret_typ, body, contract) ->
    let body_ctx, args' = List.fold_left_map (fun sctx (arg, typ) ->
      let sctx, arg' = map_binder sctx arg in
      (sctx, (arg', typ))
    ) scope_ctx args in
    let _, body' = aux body_ctx body in
    let t' = trm_fun ~annot ?loc ~ctx ?contract args' ret_typ body' in
    (* TODO: Proper function type here *)
    (scope_ctx, t')

  | Trm_typedef td ->
    (* Class namespace *)
    let class_ctx = ref (Qualified_set.empty, snd scope_ctx) in
    let body' = begin match td.typdef_body with
    | Typdef_alias _ -> td.typdef_body
    | Typdef_record rfl ->
      let rfl' = List.map (fun (rf, rf_ann) ->
        let rf' = begin match rf with
        | Record_field_method rft ->
          let (class_ctx', rft') = aux !class_ctx rft in
          class_ctx := class_ctx';
          if rft == rft' then rf else
            Record_field_method rft'
        | Record_field_member _ -> rf
        end in
        rf', rf_ann
      ) rfl in
      if List.for_all2 (==) rfl rfl' then td.typdef_body else Typdef_record rfl'
    | _ -> failwith "C_scope.map_vars: unexpected typdef_body"
    end in
    let t' = ret (body' == td.typdef_body)
      (trm_typedef ~annot ?loc ~ctx { td with typdef_body = body' })
    in
    let (_, class_var_ids) = !class_ctx in
    let class_name = td.typdef_tconstr in
    let may_update_qualifier ((qname, id): Qualified_name.t * var_id) =
      if (Qualified_map.find_opt qname (snd scope_ctx)) = (Some id)
      then (qname, id) (* this variable was the same in the outer scope *)
      else ((class_name :: fst qname, snd qname), id)
    in
    let cont_ctx = (fst scope_ctx, Qualified_map.of_seq (Seq.map may_update_qualifier (Qualified_map.to_seq class_var_ids))) in
    (cont_ctx, t')

  | _ ->
    let map_f ti =
      let _, ti = aux scope_ctx ti in
      ti
    in
    (scope_ctx, trm_map ~keep_ctx:true map_f t)

(** internal *)
let toplevel_scope_ctx (): scope_ctx = Qualified_set.empty, !toplevel_vars

(** internal *)
let check_map_var ((_conflict_set, var_ids) : scope_ctx) (annot, loc, typ, ctx, kind) var =
  let qualified = (var.qualifier, var.name) in
  if Qualified_map.find_opt qualified var_ids <> Some var.id then
    raise (InvalidVarId (sprintf "variable use %s does not satisfy C/C++ scoping rules" (var_to_string var)))
  else
    trm_var ~annot ?loc ?typ ~ctx ~kind var

(** internal *)
let check_map_binder ((conflict_set, var_ids) : scope_ctx) var =
  let qualified = (var.qualifier, var.name) in
  if Qualified_set.mem qualified conflict_set then
    raise (InvalidVarId (sprintf "redefinition of variable '%s' is illegal in C/C++" (var_to_string var)))
  else
    ((Qualified_set.add qualified conflict_set, Qualified_map.add qualified var.id var_ids), var)

(** Given term [t], check that all variable ids agree with their qualified name for C/C++ scoping rules. *)
let check_var_ids (t : trm) : unit =
  ignore (map_vars check_map_var check_map_binder (toplevel_scope_ctx ()) t)

(** internal *)
let infer_map_binder (scope_ctx : scope_ctx) var =
  let var' = if var.id = -1
    then new_var ~qualifier:var.qualifier var.name
    else var
  in
  check_map_binder scope_ctx var'

(** internal *)
let infer_map_var ((conflict_set, var_ids) : scope_ctx) (annot, loc, typ, ctx, kind) var =
  let qualified = (var.qualifier, var.name) in
  (* printf "vars: %s" (Var_set.elements) *)
  if var.id = -1 then
    trm_var ~annot ?loc ?typ ~ctx ~kind begin match Qualified_map.find_opt qualified var_ids with
    | Some id -> { qualifier = var.qualifier; name = var.name; id }
    | None -> toplevel_var ~qualifier:var.qualifier var.name
    end
  else check_map_var (conflict_set, var_ids) (annot, loc, typ, ctx, kind) var

(** Given term [t], infer variable ids such that they agree with their qualified name for C/C++ scoping rules.
  Only variable ids equal to [-1] are inferred, other ids are checked. *)
let infer_var_ids (t : trm) : trm =
  if debug then Xfile.put_contents "/tmp/ids_before.txt" (Ast_to_text.ast_to_string t);
  let _, t2 = map_vars infer_map_var infer_map_binder (toplevel_scope_ctx ()) t in
  if debug then Xfile.put_contents "/tmp/ids_after.txt" (Ast_to_text.ast_to_string t2);
  t2
