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

(** internal *)
let check_map_var (scope_ctx : scope_ctx) (annot, loc, typ, ctx, kind) var =
  let qualified = (var.qualifier, var.name) in
  match Qualified_map.find_opt qualified scope_ctx.var_ids with
  | None ->
    raise (InvalidVarId (sprintf "variable %s is used but not in scope." (var_to_string var)))
  | Some id when id <> var.id ->
    raise (InvalidVarId (sprintf "variable %s is used but variable #%d is in scope." (var_to_string var) id))
  | _ ->
    trm_var ~annot ?loc ?typ ~ctx ~kind var

(** internal *)
let check_map_binder (scope_ctx : scope_ctx) var is_predecl =
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
  ((if var.name = "" then
    (* C/C++ allows giving no variable names *)
    scope_ctx
  else if is_predecl then
    (* predeclarations don't conflict *)
    { scope_ctx with predefined = add_for_each_qualifier Qualified_set.add scope_ctx.predefined;
     var_ids = add_for_each_qualifier (fun q -> Qualified_map.add q var.id) scope_ctx.var_ids }
  else if Qualified_set.mem qualified scope_ctx.conflicts then
    raise (InvalidVarId (sprintf "redefinition of variable '%s' is illegal in C/C++" (var_to_string var)))
  else
    { scope_ctx with conflicts = add_for_each_qualifier Qualified_set.add scope_ctx.conflicts;
      predefined = add_for_each_qualifier Qualified_set.add scope_ctx.predefined;
      var_ids = add_for_each_qualifier (fun q -> Qualified_map.add q var.id) scope_ctx.var_ids }
  ), var)

(** internal *)
let enter_scope map_binder scope_ctx t =
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
          let error = "C_scope.enter_scope: expected field method" in
          let (v, _, _, _) = trm_inv ~error trm_let_fun_inv rfm in
          assert (v.qualifier = []);
          fst (map_binder scope_ctx v true)
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
  | Trm_let_fun (f_var, _, _, _, Some spec) ->
    { outer_ctx with fun_prototypes = Var_map.add f_var { ghost_args = List.map fst spec.pre.pure } outer_ctx.fun_prototypes }
  | _ -> outer_ctx

(** Given term [t], check that all variable ids agree with their qualified name for C/C++ scoping rules. *)
let check_var_ids (t : trm) : unit =
  ignore (trm_map_vars ~keep_ctx: true ~enter_scope:(enter_scope check_map_binder) ~exit_scope:scope_ctx_exit ~map_binder:check_map_binder check_map_var (toplevel_scope_ctx ()) t)

(** internal *)
let infer_map_binder (scope_ctx : scope_ctx) var t =
  let var' = if var.id = inferred_var_id
    then begin
      let qualified = (var.qualifier, var.name) in
      if Qualified_set.mem qualified scope_ctx.predefined
      then { qualifier = var.qualifier; name = var.name; id = Qualified_map.find qualified scope_ctx.var_ids }
      else new_var ~qualifier:var.qualifier var.name
    end else var
  in
  check_map_binder scope_ctx var' t

(** internal *)
let infer_map_var (scope_ctx : scope_ctx) (annot, loc, typ, ctx, kind) var =
  let qualified = (var.qualifier, var.name) in
  if var.id = inferred_var_id then
    trm_var ~annot ?loc ?typ ~ctx ~kind begin match Qualified_map.find_opt qualified scope_ctx.var_ids with
    | Some id -> { qualifier = var.qualifier; name = var.name; id }
    (* LATER: this can be confusing if triggered when not expected *)
    | None -> toplevel_var ~qualifier:var.qualifier var.name
    end
  else check_map_var scope_ctx (annot, loc, typ, ctx, kind) var

let find_prototype (scope_ctx: scope_ctx) (t: trm): fun_prototype =
  match t.desc with
  | Trm_var (_, x) ->
    begin try
      Var_map.find x scope_ctx.fun_prototypes
    with Not_found -> failwith (sprintf "Could not find a prototype for function %s" (var_to_string x))
    end
  | _ -> failwith (sprintf "Could not find a prototype for trm at location %s" (loc_to_string t.loc))

let post_process_trm (scope_ctx: scope_ctx) (t: trm): trm =
  match t.desc with
  | Trm_apps (fn, args, []) -> t
  | Trm_apps (fn, args, ghost_args) ->
    let fn_prototype = find_prototype scope_ctx fn in
    let ghost_proto_arg_map = List.fold_left (fun map ghost_var ->
      Qualified_map.add (ghost_var.qualifier, ghost_var.name) ghost_var map
    ) Qualified_map.empty fn_prototype.ghost_args in
    let rec process_ghost_args ghost_args ghost_proto_args = match ghost_args, ghost_proto_args with
    | [], _ -> []
    | (g, gval) :: gs, g_proto :: gs_proto when g == dummy_var ->
      (g_proto, gval) :: process_ghost_args gs gs_proto
    | (g, _) :: gs, [] when g == dummy_var ->
      failwith "Invalid positionnal ghost argument"
    | (g, gval) :: gs, _ when g.id = inferred_var_id ->
      let g = try Qualified_map.find (g.qualifier, g.name) ghost_proto_arg_map
        with Not_found -> failwith (sprintf "Ghost argument %s is not part of the function prototype" (var_to_string g))
      in
      (g, gval) :: process_ghost_args gs []
    | g :: gs, _ -> g :: process_ghost_args gs []
    in
    let ghost_args = process_ghost_args ghost_args fn_prototype.ghost_args in
    trm_alter ~desc:(Trm_apps (fn, args, ghost_args)) t
  | _ -> t

(** Given term [t], infer variable ids such that they agree with their qualified name for C/C++ scoping rules.
  Only variable ids equal to [inferred_var_id] are inferred, other ids are checked. *)
let infer_var_ids (t : trm) : trm =
  if debug then Xfile.put_contents "/tmp/ids_before.txt" (Ast_to_text.ast_to_string t);
  let t2 = trm_map_vars ~keep_ctx:true ~enter_scope:(enter_scope infer_map_binder) ~exit_scope:scope_ctx_exit ~map_binder:infer_map_binder ~post_process_trm infer_map_var (toplevel_scope_ctx ()) t in
  if debug then Xfile.put_contents "/tmp/ids_after.txt" (Ast_to_text.ast_to_string t2);
  t2
