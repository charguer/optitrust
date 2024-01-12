open Ast
open Trm

let debug = false

(* FIXME: triggers on multiple function declarations/definitions. *)
exception InvalidVarId of string

type fun_prototype = {
  (* args: var list; *)
  ghost_args: var list;
}

(* LATER: support overloading. here or encoding? *)
type scope_ctx = {
  prefix_qualifier_rev: string list; (** Prefix qualifier corresponding to the current namespace (if at the top level)
      example: for namespace X::Y, it is "Y"::"X"::[] *)
  conflicts: Qualified_set.t; (** Set of variables defined in the current surrounding sequence and cannot be shadowed *)
  predefined: Qualified_set.t; (** Set of variables pre-defined in the current scope that can be redefined reusing the same id *)
  var_ids: var_id Qualified_map.t; (** Map from variables to their unique ids *)
  (* constr_name / field_names *)
  fun_prototypes: fun_prototype Var_map.t; (** Map from variable storing functions to their prototype *)
}

let print_scope_ctx scope_ctx =
  printf "{\n";
  printf "  prefix_qualifier_rev = %s;\n" (Tools.list_to_string scope_ctx.prefix_qualifier_rev);
  printf "  conflicts = %s;\n" (
    vars_to_string (
    List.of_seq (
    Seq.map (fun (q, n) -> name_to_var ~qualifier:q n) (
    Qualified_set.to_seq scope_ctx.conflicts))));
  printf "  predefined = %s;\n" (
    vars_to_string (
    List.of_seq (
    Seq.map (fun (q, n) -> name_to_var ~qualifier:q n) (
    Qualified_set.to_seq scope_ctx.predefined))));
  printf "var_ids = %s;\n" (
    vars_to_string (
    List.of_seq (
    Seq.map (fun ((q, n), id) -> { qualifier = q; name = n; id = id }) (
    Qualified_map.to_seq scope_ctx.var_ids))));
  printf "}\n"

(** internal *)
let toplevel_scope_ctx (): scope_ctx = {
  prefix_qualifier_rev = [];
  conflicts = Qualified_set.empty;
  predefined = Qualified_map.fold (fun qname _ set -> Qualified_set.add qname set) !toplevel_vars Qualified_set.empty;
  var_ids = !toplevel_vars;
  fun_prototypes = Var_map.empty;
}

(* LATER: #var-id, flag to disable check for performance *)
(* cost: traverse the AST in O(n) and O(m log m) where m is the number of binders. *)
(* TODO: raise error or ignore the dummy ids (-1) *)
let check_unique_var_ids (t : trm) : unit =
  (* LATER: refactor with function mapping over bindings? *)
  let vars = ref Var_set.empty in
  let add_var v =
    if Var_set.mem v !vars then
      failwith (sprintf "variable '%s' is not declared with a unique id" (var_to_string v));
    vars := Var_set.add v !vars
  in
  let rec aux t =
    begin match t.desc with
    | Trm_let (_, (x, _), _) ->
      add_var x
    | Trm_let_mult (_, tvs, _) ->
      List.iter (fun (x, _) -> add_var x) tvs
    | Trm_let_fun (x, _, _, _, _)
      (* FIXME: kind of C-specific? *)
      when not (Trm.is_fun_with_empty_body t) ->
      add_var x
    | Trm_for ((x, _, _, _, _, _), _, _) ->
      add_var x
    (* | Trm_typedef td -> *)
    | _ -> ()
    end;
    trm_iter aux t
  in
  aux t

(** internal *)
let check_var (scope_ctx : scope_ctx) var =
  let qualified = (var.qualifier, var.name) in
  match Qualified_map.find_opt qualified scope_ctx.var_ids with
  | None ->
    raise (InvalidVarId (sprintf "variable %s is used but not in scope." (var_to_string var)))
  | Some id when id <> var.id ->
    raise (InvalidVarId (sprintf "variable %s is used but variable #%d is in scope." (var_to_string var) id))
  | _ -> ()

(** internal *)
let check_binder ?(allow_redefinition = false) (scope_ctx : scope_ctx) var is_predecl =
  let qualified = (var.qualifier, var.name) in
  (* currently we add all possible qualifiers to the data structures,
     the alternative is to add only one qualifier (absolute?) and to
     perform more complex .mem checks taking qualifiers into account. *)
  let add_for_each_qualifier obj_add obj =
    let qualifier = ref var.qualifier in
    List.fold_left (fun obj q ->
      qualifier := q :: !qualifier;
      obj_add (!qualifier, var.name) obj
    ) (obj_add qualified obj) scope_ctx.prefix_qualifier_rev
  in
  (if var.name = "" then
    (* C/C++ allows giving no variable names *)
    scope_ctx
  else if is_predecl then
    (* predeclarations don't conflict *)
    { scope_ctx with predefined = add_for_each_qualifier Qualified_set.add scope_ctx.predefined;
     var_ids = add_for_each_qualifier (fun q -> Qualified_map.add q var.id) scope_ctx.var_ids }
  else if (not allow_redefinition) && Qualified_set.mem qualified scope_ctx.conflicts then
    raise (InvalidVarId (sprintf "redefinition of variable '%s' is illegal in C/C++" (var_to_string var)))
  else
    { scope_ctx with conflicts = add_for_each_qualifier Qualified_set.add scope_ctx.conflicts;
      predefined = add_for_each_qualifier Qualified_set.add scope_ctx.predefined;
      var_ids = add_for_each_qualifier (fun q -> Qualified_map.add q var.id) scope_ctx.var_ids }
  )

let find_prototype (scope_ctx: scope_ctx) (t: trm): fun_prototype =
  match t.desc with
  | Trm_var (_, x) ->
    begin try
      Var_map.find x scope_ctx.fun_prototypes
    with Not_found -> failwith (sprintf "Could not find a prototype for function %s" (var_to_string x))
    end
  | _ -> failwith (sprintf "Could not find a prototype for trm at location %s" (loc_to_string t.loc))

let on_ghost_arg_name (scope_ctx: scope_ctx) (fn: trm)
  (f : var Qualified_map.t -> var list ref -> 'a) : 'a =
  let fn_prototype = find_prototype scope_ctx fn in
  let ghost_proto_arg_map = List.fold_left (fun map ghost_var ->
    Qualified_map.add (ghost_var.qualifier, ghost_var.name) ghost_var map
  ) Qualified_map.empty fn_prototype.ghost_args in
  let ghost_args_proto = ref fn_prototype.ghost_args in
  f ghost_proto_arg_map ghost_args_proto

let check_ghost_arg_name_aux (ghost_proto_arg_map : var Qualified_map.t) (_ : var list ref)
  (g : var) =
  try
    let g' = Qualified_map.find (g.qualifier, g.name) ghost_proto_arg_map in
    if g.id <> g'.id then
      failwith (sprintf "Ghost argument %s is not the same as the ghost in the prototype %s." (var_to_string g) (var_to_string g'))
  with Not_found -> failwith (sprintf "Ghost argument %s is not part of the function prototype" (var_to_string g))

let check_ghost_arg_name (scope_ctx: scope_ctx) (fn: trm) : hyp -> unit =
  on_ghost_arg_name scope_ctx fn check_ghost_arg_name_aux

(** internal *)
let enter_scope check_binder scope_ctx t =
  match t.desc with
  | Trm_namespace (name, _, _) ->
    (* TODO: conflicts ~= filter_map is_qualified conflicts *)
    { scope_ctx with prefix_qualifier_rev = name :: scope_ctx.prefix_qualifier_rev; conflicts = Qualified_set.empty; predefined = Qualified_set.empty }
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_alias _ -> scope_ctx
    | Typdef_record rfl ->
      let scope_ctx = { scope_ctx with prefix_qualifier_rev = td.typdef_tconstr :: scope_ctx.prefix_qualifier_rev; conflicts = Qualified_set.empty; predefined = Qualified_set.empty } in
      (* order of declaration does not matter for class members:
         this is equivalent to implicit predefinitions. *)
      List.fold_left (fun scope_ctx (rf, _) ->
        begin match rf with
        (* TODO: also predefine members *)
        | Record_field_member _ -> scope_ctx
        | Record_field_method rfm ->
          let error = "Scope_computation.enter_scope: expected field method" in
          let (v, _, _, _) = trm_inv ~error trm_let_fun_inv rfm in
          assert (v.qualifier = []);
          check_binder scope_ctx v true
        end
      ) scope_ctx rfl
    | _ -> failwith "unexpected typdef_body"
    end
  | _ ->
    { scope_ctx with prefix_qualifier_rev = []; conflicts = Qualified_set.empty; predefined = Qualified_set.empty }

(** internal *)
let scope_ctx_exit outer_ctx inner_ctx t =
  match t.desc with
  | Trm_namespace (name, _, _)
  | Trm_typedef { typdef_tconstr = name } ->
    (* q1::q2::...::qN::n, is qualified by N if q1 = N
      example:
      namespace q0 { namespace q2 { void f(); }}
      namespace q1 { void f(); namespace q2 { void f() { return q0::q2::f(); } }}
      [q0::q2::f; q1::f; f; q2::f; q1::q2::f;] --exit q2--> prefix_qualifier_rev = q2::q1::[]
      [q0::q2::f; q1::f; q2::f; q1::q2::f] --exit q1--> prefix_qualifier_rev = q1::[]
      [q0::q2::f; q1::f; q1::q2::f] *)
    let rec is_qualified ?(prefix_qualifier_rev = inner_ctx.prefix_qualifier_rev) ((q, n): typconstr)  : bool =
      match (q, prefix_qualifier_rev) with
      (* Case 1. qualifier starts with current namespace *)
      | (fq :: _, pq :: _) when fq = pq -> true
      (* Case 2. qualifier starts with other namespace *)
      | (_, _ :: pqr) -> is_qualified (q, n) ~prefix_qualifier_rev:pqr
      | _ -> false
    in
    let union_var_ids (q, n) a b =
      if debug then begin
        printf "outer:\n";
        print_scope_ctx outer_ctx;
        printf "inner:\n";
        print_scope_ctx inner_ctx;
      end;
      raise (InvalidVarId (sprintf "variable '%s' is defined both inside and outside of namespace" (var_to_string (name_to_var ~qualifier:q n))))
    in
    { prefix_qualifier_rev = outer_ctx.prefix_qualifier_rev;
      (* conflict in inner_ctx: [f; N::f], keep [N::f] conflict. *)
      conflicts = Qualified_set.union outer_ctx.conflicts (Qualified_set.filter is_qualified (inner_ctx.conflicts));
      predefined = Qualified_set.union outer_ctx.predefined (Qualified_set.filter is_qualified (inner_ctx.predefined));
      var_ids = Qualified_map.union union_var_ids outer_ctx.var_ids (Qualified_map.filter (fun k v -> is_qualified k) inner_ctx.var_ids);
      fun_prototypes = Var_map.union (fun x _ _ ->
        raise (InvalidVarId (sprintf "variable '%s' has a function specification both inside and outside the namespace" (var_to_string x))))
        outer_ctx.fun_prototypes (Var_map.filter (fun x _ -> is_qualified (x.qualifier, x.name)) inner_ctx.fun_prototypes) }
  | _ -> outer_ctx

let post_process_ctx ctx t =
  match t.desc with
  | Trm_let_fun (f_var, _, _, _, FunSpecContract spec) ->
    { ctx with fun_prototypes = Var_map.add f_var { ghost_args = List.map fst spec.pre.pure } ctx.fun_prototypes }
  | Trm_let_fun (f_var, _, _, _, FunSpecReverts f_reverted) ->
    check_var ctx f_reverted;
    begin match Var_map.find_opt f_reverted ctx.fun_prototypes with
    | Some proto -> { ctx with fun_prototypes = Var_map.add f_var proto ctx.fun_prototypes }
    | None -> failwith (sprintf "Function %s cannot revert %s because its contract is undefined" (var_to_string f_var) (var_to_string f_reverted))
    end
  | _ -> ctx

(** Given term [t], check that all variable ids agree with their qualified name for C/C++ scoping rules. *)
let check_var_ids ?(check_uniqueness = true) (t : trm) : unit =
  trm_iter_vars ~enter_scope:(enter_scope check_binder) ~exit_scope:scope_ctx_exit ~post_process:post_process_ctx ~iter_binder:check_binder ~iter_ghost_arg_name:check_ghost_arg_name check_var (toplevel_scope_ctx ()) t;
  if check_uniqueness then check_unique_var_ids t

(** internal *)
let infer_map_binder ?(allow_redefinition = false) (scope_ctx : scope_ctx) var t =
  let var' = if var.id = inferred_var_id
    then begin
      let qualified = (var.qualifier, var.name) in
      if Qualified_set.mem qualified scope_ctx.predefined
      then { qualifier = var.qualifier; name = var.name; id = Qualified_map.find qualified scope_ctx.var_ids }
      else new_var ~qualifier:var.qualifier var.name
    end else var
  in
  (check_binder ~allow_redefinition scope_ctx var' t, var')

(** internal *)
let infer_map_var ?(check = true) (scope_ctx : scope_ctx) var =
  let qualified = (var.qualifier, var.name) in
  if var.id = inferred_var_id then
    match Qualified_map.find_opt qualified scope_ctx.var_ids with
    | Some id -> { qualifier = var.qualifier; name = var.name; id }
    (* LATER: this can be confusing if triggered when not expected *)
    | None -> toplevel_var ~qualifier:var.qualifier var.name
  else begin
    if check then check_var scope_ctx var;
    var
  end

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
    match Qualified_map.find_opt (g.qualifier, g.name) ghost_proto_arg_map with
    | Some g' -> g'
    | None -> g (* inference failed, but leaving this for check *)
  end

(* TODO: assymmetric with other functions, use flag? *)
let only_infer_ghost_arg_name (scope_ctx: scope_ctx) (fn: trm) : hyp -> var =
  on_ghost_arg_name scope_ctx fn only_infer_ghost_arg_name_aux

let infer_ghost_arg_name (scope_ctx: scope_ctx) (fn: trm) : hyp -> var =
  on_ghost_arg_name scope_ctx fn (fun gpam gap v ->
    let v' = only_infer_ghost_arg_name_aux gpam gap v in
    check_ghost_arg_name_aux gpam gap v';
    v'
  )

(** Given term [t], infer variable ids such that they agree with their qualified name for C/C++ scoping rules.
  Only variable ids equal to [inferred_var_id] are inferred, other ids are checked. *)
let infer_var_ids ?(check = true) ?(check_uniqueness = check) (t : trm) : trm =
  let allow_redefinition = not check in
  let t2 = trm_rename_vars ~keep_ctx:true
    ~enter_scope:(enter_scope (fun ctx binder predecl -> fst (infer_map_binder ~allow_redefinition ctx binder predecl)))
    ~exit_scope:scope_ctx_exit
    ~post_process:(fun ctx t -> (post_process_ctx ctx t, t))
    ~map_binder:(infer_map_binder ~allow_redefinition)
    ~map_ghost_arg_name:(if check then infer_ghost_arg_name else only_infer_ghost_arg_name)
    (infer_map_var ~check)
    (toplevel_scope_ctx ()) t in
  if check_uniqueness then check_unique_var_ids t2;
  t2
