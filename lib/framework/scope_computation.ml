open Ast
open Trm

let debug = false

(* FIXME: triggers on multiple function declarations/definitions. *)
exception InvalidVarId of string

type fun_prototype = {
  (* args: var list; *)
  ghost_args: (var * bool) list; (* ghost_name, is_implicit *)
}

(* LATER: support overloading. here or encoding? *)
type scope_ctx = {
  namespace_path_rev: string list; (** Current namespace path for identifiers
      example: for namespace X::Y, it is "Y"::"X"::[] *)
  conflicts: Qualified_set.t; (** Set of variables defined in the current surrounding sequence and cannot be shadowed *)
  predefined: Qualified_set.t; (** Set of variables pre-defined in the current scope that can be redefined reusing the same id *)
  var_ids: var_id Qualified_map.t; (** Map from variables to their unique ids *)
  (* constr_name / field_names *)
  fun_prototypes: fun_prototype Var_map.t; (** Map from variable storing functions to their prototype *)
  toplevel: bool; (** A boolean to indicate if we are at toplevel or not. Variables declared at toplevel always get toplevel identifiers with negative ids. *)

  shadowed: var Var_map.t; (** Shadowed variables, using them triggers a rename. *)
  renames: string Var_map.t ref; (** When identfiers are correct, but the name needs to be changed. This field is a global accumulator. *)
}

let print_scope_ctx scope_ctx =
  Tools.debug "{";
  Tools.debug "  namespace_path_rev = %s;" (Tools.list_to_string scope_ctx.namespace_path_rev);
  Tools.debug "  conflicts = %s;" (
    vars_to_string (
    List.of_seq (
    Seq.map (fun (q, n) -> name_to_var ~namespaces:q n) (
    Qualified_set.to_seq scope_ctx.conflicts))));
  Tools.debug "  predefined = %s;" (
    vars_to_string (
    List.of_seq (
    Seq.map (fun (q, n) -> name_to_var ~namespaces:q n) (
    Qualified_set.to_seq scope_ctx.predefined))));
  Tools.debug "  var_ids = %s;" (
    vars_to_string (
    List.of_seq (
    Seq.map (fun ((q, n), id) -> { namespaces = q; name = n; id = id }) (
    Qualified_map.to_seq scope_ctx.var_ids))));
  Tools.debug "  shadowed = %s;" (
    Tools.list_to_string (
    List.of_seq (
    Seq.map (fun (v, n) -> sprintf "%s is shadowed by %s" (var_to_string v) (var_to_string n)) (
    Var_map.to_seq scope_ctx.shadowed)))
  );
  Tools.debug "  renames = %s;" (
    Tools.list_to_string (
    List.of_seq (
    Seq.map (fun (v, n) -> sprintf "%s -> %s" (var_to_string v) n) (
    Var_map.to_seq !(scope_ctx.renames))))
  );
  Tools.debug "}"

(** internal *)
let toplevel_scope_ctx (): scope_ctx = {
  namespace_path_rev = [];
  conflicts = Qualified_set.empty;
  predefined = Qualified_set.empty;
  var_ids = Qualified_map.empty;
  fun_prototypes = Var_map.empty;
  toplevel = true;
  shadowed = Var_map.empty;
  renames = ref Var_map.empty;
}

(* LATER: #var-id, flag to disable check for performance *)
(* cost: traverse the AST in O(n) and O(m log m) where m is the number of binders. *)
(* TODO: raise error or ignore the dummy ids (-1) *)
let check_unique_var_ids (t : trm) : unit =
  (* FIXME: This does not catch all the duplicated variable ids. Especially in contracts. *)
  (* LATER: refactor with function mapping over bindings? *)
  let vars = ref Var_set.empty in
  let add_var v =
    if Var_set.mem v !vars then
      failwith "variable '%s' is not declared with a unique id" (var_to_string v);
    vars := Var_set.add v !vars
  in
  let rec aux t =
    begin match t.desc with
    | Trm_let ((x, _), body) ->
      add_var x
    | Trm_let_mult bs ->
      List.iter (fun ((x, _), _) -> add_var x) bs
    | Trm_for (range, _, _) ->
      add_var range.index
    (* | Trm_typedef td -> *)
    | _ -> ()
    end;
    trm_iter aux t
  in
  aux t

(** raise an [InvalidVarId] error. *)
let raise_invalid_var_id (error : string) : unit =
  raise (InvalidVarId error)

(** internal *)
let scope_ctx_record_rename ~failure_allowed var scope_ctx =
  (* DEBUG: printf "renaming %s\n" (var_to_string var); *)
  (* We should never ask to rename toplevel variables since they should never conflict. *)
  if failure_allowed && is_toplevel_var var then raise_invalid_var_id (sprintf "cannot rename the toplevel variable %s" var.name);
  (* FIXME: fresh_var_name should be deterministic based on visible scope / free vars. *)
  scope_ctx.renames := Var_map.add var (fresh_var_name ~prefix:var.name ()) !(scope_ctx.renames)

(** internal *)
let infer_map_var ~(failure_allowed : bool) (scope_ctx : scope_ctx) var =
  let qualified = (var.namespaces, var.name) in
  let infer_var_id () =
    (* Case 1: infer var id according to name. *)
    match Qualified_map.find_opt qualified scope_ctx.var_ids with
    | Some id -> { namespaces = var.namespaces; name = var.name; id }
    (* If the variable is not found in the current context, it should be a toplevel variable.
       This can be confusing if triggered when not expected.
       In particular if we are inside a namespace, we do not take this into account and consider that the variable has an absolute namespace path. *)
    | None -> toplevel_var ~namespaces:var.namespaces var.name
  in
  let check_var_id () =
    if failure_allowed then begin
      (* Case 3: check var id consisent with name. *)
      if is_toplevel_var var then
        begin if not (var_eq (toplevel_var ~namespaces:var.namespaces var.name) var) then
          raise (InvalidVarId (sprintf "toplevel variable %s has a wrong id" (var_to_string var)))
        end
      else if is_anon_var var then begin
        if var.id = 0 then raise (InvalidVarId "found anonymous variable without id")
      end else
        begin match Qualified_map.find_opt qualified scope_ctx.var_ids with
        | None ->
          raise (InvalidVarId
            (sprintf "variable %s is used but not in scope." (var_to_string var)))
        | Some id when (id <> var.id) && not (Var_map.mem var scope_ctx.shadowed) ->
          raise (InvalidVarId (
            (sprintf "variable %s is used but variable #%d is in scope." (var_to_string var) id)))
        | _ -> ()
        end
    end;
    var
  in
  let var' = if has_unset_id var
    then infer_var_id ()
    else check_var_id ()
  in
  let rec rename_shadows v scope_ctx =
    (* We need to rename not only the directly shadowed variable but also recursively the variables that are shadowing it:
       Ex: On (fun x#1 x#2 x#3. x#1), x#1 triggers a renaming on x#2 which itself is shadowed by x#3 so both x#2 and x#3 need to be renamed. *)
    match Var_map.find_opt v scope_ctx.shadowed with
    | None -> ()
    | Some v_shadow ->
      if not (Var_map.mem v_shadow !(scope_ctx.renames))
        then scope_ctx_record_rename ~failure_allowed v_shadow scope_ctx;
      rename_shadows v_shadow scope_ctx
  in
  rename_shadows var' scope_ctx;
  var'

(** Auxiliary function for applying [infer_map_binder] in the specific case
    of variables bound at top-level; for such variables, an entry is added/updated
    in the global map [Trm.toplevel_vars]. *)
let infer_id_for_toplevel_var var scope_ctx =
  let namespaces =
    match var.namespaces with
    | "" :: namespaces -> namespaces
    | namespaces -> List.rev_append scope_ctx.namespace_path_rev namespaces
    in
  if namespaces = [] then
    (* Small optimization for hash-consing when there is no namespace *)
    toplevel_var var.name
  else
    let toplevel_var = toplevel_var ~namespaces var.name in
    { toplevel_var with namespaces = var.namespaces }

(** internal *)
let infer_map_binder ~(failure_allowed : bool) (scope_ctx : scope_ctx)
  var is_predecl : scope_ctx * var =
  let qualified = (var.namespaces, var.name) in
  let infer_var_id () =
    if Qualified_set.mem qualified scope_ctx.predefined then
      { namespaces = var.namespaces;
        name = var.name;
        id = Qualified_map.find qualified scope_ctx.var_ids }
    else if scope_ctx.toplevel then
      infer_id_for_toplevel_var var scope_ctx
    else new_var ~namespaces:var.namespaces var.name
  in
  (* currently we add all possible namespaces paths to the data structures,
  the alternative is to add only one path (absolute?) and to
  perform more complex .mem checks taking namespaces into account. *)
  (* FIXME: This is broken for absolute paths *)
  let add_for_each_namespace_suffix var obj_add obj =
    let namespaces = ref var.namespaces in
    List.fold_left (fun obj q ->
      namespaces := q :: !namespaces;
      obj_add (!namespaces, var.name) obj
    ) (obj_add qualified obj) scope_ctx.namespace_path_rev
  in
  (* 1. infer var id if necessary *)
  let var = if has_unset_id var then infer_var_id () else var in
  if is_anon_var var then
    (* 2. Ignore anonymous variables *)
    (scope_ctx, var)
  else if is_predecl then
    (* 3. predeclarations don't conflict *)
    (* TODO: handle combination with renaming *)
    ({ scope_ctx with predefined = add_for_each_namespace_suffix var Qualified_set.add scope_ctx.predefined;
     var_ids = add_for_each_namespace_suffix var (fun q -> Qualified_map.add q var.id) scope_ctx.var_ids },
     var)
  else begin
    (* 4. fix redefinitions or shadowing conflicts by renaming later on. *)
    if Qualified_set.mem qualified scope_ctx.conflicts
      then scope_ctx_record_rename ~failure_allowed var scope_ctx;
    let scope_ctx = match Qualified_map.find_opt qualified scope_ctx.var_ids with
    | Some id when id <> var.id ->
      { scope_ctx with shadowed = Var_map.add { namespaces = []; name = ""; id } var scope_ctx.shadowed }
    | _ -> scope_ctx
    in
    (* 5. normal case *)
    ({ scope_ctx with conflicts = add_for_each_namespace_suffix var Qualified_set.add scope_ctx.conflicts;
    predefined = add_for_each_namespace_suffix var Qualified_set.add scope_ctx.predefined;
    var_ids = add_for_each_namespace_suffix var (fun q -> Qualified_map.add q var.id) scope_ctx.var_ids },
    var)
  end

let find_prototype (scope_ctx: scope_ctx) (t: trm): fun_prototype =
  match t.desc with
  | Trm_var x ->
    begin try
      Var_map.find x scope_ctx.fun_prototypes
    with
    | Not_found when var_eq x Resource_trm.var_admitted -> { ghost_args = [Resource_trm.var_justif, false] }
    | Not_found when var_eq x Resource_trm.var_assert_alias -> Var_map.find Resource_trm.var_assert_eq scope_ctx.fun_prototypes
    | Not_found -> failwith "Could not find a prototype for function %s" (var_to_string x)
    end
  | _ -> failwith "Could not find a prototype for trm at location %s" (loc_to_string t.loc)

let on_ghost_arg_name (scope_ctx: scope_ctx) (fn: trm)
  (f : var Qualified_map.t -> var list ref -> 'a) : 'a =
  let fn_prototype = find_prototype scope_ctx fn in
  let ghost_proto_arg_map = List.fold_left (fun map (ghost_var, _) ->
    Qualified_map.add (ghost_var.namespaces, ghost_var.name) ghost_var map
  ) Qualified_map.empty fn_prototype.ghost_args in
  let ghost_args_proto = ref (List.filter_map (fun (ghost_var, is_implicit) -> if is_implicit then None else Some ghost_var) fn_prototype.ghost_args) in
  f ghost_proto_arg_map ghost_args_proto

let check_ghost_arg_name_aux (ghost_proto_arg_map : var Qualified_map.t) (_ : var list ref)
  (g : var) =
  try
    let g' = Qualified_map.find (g.namespaces, g.name) ghost_proto_arg_map in
    if g.id <> g'.id then
      failwith "Ghost argument %s is not the same as the ghost in the prototype %s." (var_to_string g) (var_to_string g')
  with Not_found -> failwith "Ghost argument %s is not part of the function prototype" (var_to_string g)

let check_ghost_arg_name (scope_ctx: scope_ctx) (fn: trm) : var -> unit =
  on_ghost_arg_name scope_ctx fn check_ghost_arg_name_aux

(** internal *)
let enter_scope check_binder scope_ctx t =
  match t.desc with
  | Trm_namespace (name, _, _) ->
    (* TODO: conflicts ~= filter_map is_qualified conflicts *)
    { scope_ctx with namespace_path_rev = name :: scope_ctx.namespace_path_rev; conflicts = Qualified_set.empty; predefined = Qualified_set.empty; }
  | Trm_typedef td ->
    begin match td.typedef_body with
    | Typedef_alias _ -> scope_ctx
    (* TODO: Typedef_union  for each constructor C,
       we need to add a function in scope for C and for __Pattern_C
       need to return scope_ctx with two additions in it
       returns something like

           ({ scope_ctx with conflicts =
            var_ids = Qualified_map.add ... scope_ctx.var_ids }

          and leaving a 'later' for handling workspaces
       *)
    | Typedef_record rfl ->
      (* TODO: should be not do a scope for union, so that they bind types and cstr at toplevel? *)
      let scope_ctx = { scope_ctx with namespace_path_rev = td.typedef_name.name :: scope_ctx.namespace_path_rev; conflicts = Qualified_set.empty; predefined = Qualified_set.empty; } in
      (* order of declaration does not matter for class members:
         this is equivalent to implicit predefinitions. *)
      List.fold_left (fun scope_ctx (rf, _) ->
        begin match rf with
        (* TODO: also predefine fields *)
        | Record_field _ -> scope_ctx
        | Record_method rfm ->
          let error = "Scope_computation.enter_scope: expected field method" in
          let (v, _, _, _, _) = trm_inv ~error trm_let_fun_inv rfm in
          assert (v.namespaces = []);
          check_binder scope_ctx v true
        end
      ) scope_ctx rfl
    | Typedef_union _cstrs -> scope_ctx
    | _ -> failwith "unexpected typedef_body"
    end
  | _ ->
    { scope_ctx with namespace_path_rev = []; toplevel = false; conflicts = Qualified_set.empty; predefined = Qualified_set.empty; }

(** internal *)
let scope_ctx_exit outer_ctx inner_ctx t =
  (* TODO: handle ~failure_allowed *)
  match t.desc with
  | Trm_namespace (name, _, _)
  | Trm_typedef { typedef_name = { name } } ->
    (* q1::q2::...::qN::n, is qualified by N if q1 = N
      example:
      namespace q0 { namespace q2 { void f(); }}
      namespace q1 { void f(); namespace q2 { void f() { return q0::q2::f(); } }}
      [q0::q2::f; q1::f; f; q2::f; q1::q2::f;] --exit q2--> namespace_path_rev = q2::q1::[]
      [q0::q2::f; q1::f; q2::f; q1::q2::f] --exit q1--> namespace_path_rev = q1::[]
      [q0::q2::f; q1::f; q1::q2::f] *)
    let rec is_qualified ?(namespace_path_rev = inner_ctx.namespace_path_rev) ((q, n): Qualified_name.t)  : bool =
      match (q, namespace_path_rev) with
      (* Case 1. qualifier starts with current namespace *)
      | (fq :: _, pq :: _) when fq = pq -> true
      (* Case 2. qualifier starts with other namespace *)
      | (_, _ :: pqr) -> is_qualified (q, n) ~namespace_path_rev:pqr
      | _ -> false
    in
    let union_var_ids (q, n) a b =
      if debug then begin
        Tools.debug "outer:";
        print_scope_ctx outer_ctx;
        Tools.debug "inner:";
        print_scope_ctx inner_ctx;
      end;
      raise (InvalidVarId (sprintf "variable '%s' is defined both inside and outside of namespace" (var_to_string (name_to_var ~namespaces:q n))))
    in
    { namespace_path_rev = outer_ctx.namespace_path_rev;
      (* conflict in inner_ctx: [f; N::f], keep [N::f] conflict. *)
      conflicts = Qualified_set.union outer_ctx.conflicts (Qualified_set.filter is_qualified (inner_ctx.conflicts));
      predefined = Qualified_set.union outer_ctx.predefined (Qualified_set.filter is_qualified (inner_ctx.predefined));
      var_ids = Qualified_map.union union_var_ids outer_ctx.var_ids (Qualified_map.filter (fun k v -> is_qualified k) inner_ctx.var_ids);
      fun_prototypes = Var_map.union (fun x _ _ ->
        raise (InvalidVarId (sprintf "variable '%s' has a function specification both inside and outside the namespace" (var_to_string x))))
        outer_ctx.fun_prototypes (Var_map.filter (fun x _ -> is_qualified (x.namespaces, x.name)) inner_ctx.fun_prototypes);
      toplevel = outer_ctx.toplevel;
      shadowed = outer_ctx.shadowed;
      renames = inner_ctx.renames; }
  | _ -> { outer_ctx with renames = inner_ctx.renames }

let post_process_ctx ~(failure_allowed : bool) ctx t =
  match trm_let_fun_inv t with
  | Some (f_var, _, _, _, FunSpecContract spec) ->
    { ctx with fun_prototypes = Var_map.add f_var { ghost_args = List.map (fun (arg_var, arg_typ) -> (arg_var, Typ.is_typ_type arg_typ)) spec.pre.pure } ctx.fun_prototypes }
  | Some (f_var, _, _, _, FunSpecReverts f_reverted) ->
    let f_reverted = infer_map_var ~failure_allowed ctx f_reverted in
    begin match Var_map.find_opt f_reverted ctx.fun_prototypes with
    | Some proto -> { ctx with fun_prototypes = Var_map.add f_var proto ctx.fun_prototypes }
    | None -> failwith "Function %s cannot revert %s because its contract is undefined" (var_to_string f_var) (var_to_string f_reverted)
    end
  | _ -> ctx

let only_infer_ghost_arg_name_aux (ghost_proto_arg_map : var Qualified_map.t)
  (ghost_args_proto : var list ref)
  (g : var) =
  if g == dummy_var then
    match !ghost_args_proto with
    | [] -> failwith "Invalid positional ghost argument"
    | g_proto :: gs_proto ->
      ghost_args_proto := gs_proto;
      g_proto
  else begin
    ghost_args_proto := [];
    match Qualified_map.find_opt (g.namespaces, g.name) ghost_proto_arg_map with
    | Some g' -> g'
    | None -> g (* inference failed, but leaving this for check *)
  end

(* TODO: assymmetric with other functions, use flag? *)
let only_infer_ghost_arg_name (scope_ctx: scope_ctx) (fn: trm) : var -> var =
  on_ghost_arg_name scope_ctx fn only_infer_ghost_arg_name_aux

let infer_ghost_arg_name (scope_ctx: scope_ctx) (fn: trm) : var -> var =
  on_ghost_arg_name scope_ctx fn (fun gpam gap v ->
    let v' = only_infer_ghost_arg_name_aux gpam gap v in
    check_ghost_arg_name_aux gpam gap v';
    v'
  )

(** Perform a renaming on certain variable names, as guided by [renames].
    See ctx.renames. *)
let rename_vars_if_needed (t : trm) (renames : string Var_map.t) =
  let may_rename (v : var) : var =
    match Var_map.find_opt v renames with
    | Some name -> { namespaces = v.namespaces; name; id = v.id }
    | None -> v
  in
  let map_var () v = may_rename v in
  let map_binder () v _ = ((), may_rename v) in
  if Var_map.is_empty renames then t
  else trm_rename_vars map_var ~map_binder () t

(** Given term [t] describing the root of an AST,
    infer variable ids such that they agree with their qualified name for C/C++ scoping rules,
    and return a modified term in which all variables (binders and occurrences) have an id.
    Only variable ids equal to [inferred_var_id] are inferred, other ids are checked.

    If variables of the input term already have IDs, these IDs are preserved.
    If the option check_uniqueness is used, the function checks that every binder
    binds a unique ID.

    If variable names would not have resolved to the IDs already stored in the ast,
    then the variables are renamed (see ctx.renames). This situation can happen
    for example if unrolling a loop, a variable bound inside the loop now has
    multiple occurrences with the same name, which is problematic as shadowing is disallowed.*)
let infer_var_ids ?(failure_allowed = true) ?(check_uniqueness = not failure_allowed) (t : trm) : trm =
  let ctx, t2 = trm_rename_vars_ret_ctx ~keep_ctx:true
    ~enter_scope:(enter_scope (fun ctx binder predecl -> fst (infer_map_binder ~failure_allowed ctx binder predecl)))
    ~exit_scope:scope_ctx_exit
    ~post_process:(fun ctx t -> (post_process_ctx ~failure_allowed ctx t, t))
    ~map_binder:(infer_map_binder ~failure_allowed)
    ~map_ghost_arg_name:(if failure_allowed then only_infer_ghost_arg_name else infer_ghost_arg_name)
    (infer_map_var ~failure_allowed)
    (toplevel_scope_ctx ()) t in
  if check_uniqueness then check_unique_var_ids t2;
  rename_vars_if_needed t2 !(ctx.renames)
