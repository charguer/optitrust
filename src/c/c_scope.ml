open Ast
open Trm

let debug = true

(* FIXME: triggers on multiple function declarations/definitions. *)
exception InvalidVarId of string

(** Prefix qualifier corresponding to the current namespace,
    Set of variables defined in the current surrounding sequence,
    Set of variables pre-defined in the current scope,
    Map from variables to their unique ids. *)
type scope_ctx = {
  prefix_qualifier_rev: string list;
  conflicts: Qualified_set.t;
  predefined: Qualified_set.t;
  var_ids: var_id Qualified_map.t;
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
  predefined = Qualified_set.empty; (* LATER: could add some toplevel vars that we allow redefining here. *)
  var_ids = !toplevel_free_vars;
}

(** internal *)
let check_map_var (scope_ctx : scope_ctx) (annot, loc, typ, ctx, kind) var =
  let qualified = (var.qualifier, var.name) in
  match Qualified_map.find_opt qualified scope_ctx.var_ids with
  | None ->
    raise (InvalidVarId (sprintf "variable %s is used but not in scope." (var_to_string var)))
  | Some id when id <> var.id ->
    raise (InvalidVarId (sprintf "variable %s is used but variable #%d is in scope." (var_to_string var) var.id))
  | _ ->
    trm_var ~annot ?loc ?typ ~ctx ~kind var

(** internal *)
let check_map_binder (scope_ctx : scope_ctx) var t =
  let qualified = (var.qualifier, var.name) in
  (* currently we add all possible qualifiers to the data structures,
     the alternative is to add only one qualifier (absolute?) and to
     perform more complex .mem checks taking qualifiers into account. *)
  let add_for_each_qualifier obj_add obj =
    let qualifier = ref var.qualifier in
    let prefix_qualifier_rev =
      (* FIXME? assuming only and all functions are top-level in the current namespace. *)
      if Option.is_some (Option.bind t trm_let_fun_inv) then
        scope_ctx.prefix_qualifier_rev
      else [] in
    List.fold_left (fun obj q ->
      qualifier := q :: !qualifier;
      obj_add (!qualifier, var.name) obj
    ) (obj_add qualified obj) prefix_qualifier_rev
  in
  ((if var.name = "" then
    (* C/C++ allows giving no variable names *)
    scope_ctx
  else if Qualified_set.mem qualified scope_ctx.conflicts then
    raise (InvalidVarId (sprintf "redefinition of variable '%s' is illegal in C/C++" (var_to_string var)))
  else if (Option.map Trm.is_fun_with_empty_body t) = (Some true) then
    (* predeclarations don't conflict *)
    { scope_ctx with predefined = add_for_each_qualifier Qualified_set.add scope_ctx.predefined;
     var_ids = add_for_each_qualifier (fun q -> Qualified_map.add q var.id) scope_ctx.var_ids }
  else
    { scope_ctx with conflicts = add_for_each_qualifier Qualified_set.add scope_ctx.conflicts;
      var_ids = add_for_each_qualifier (fun q -> Qualified_map.add q var.id) scope_ctx.var_ids }
  ), var)

(** internal *)
let enter_scope map_binder scope_ctx t =
  match t.desc with
  | Trm_namespace (name, _, _) ->
    { scope_ctx with prefix_qualifier_rev = name :: scope_ctx.prefix_qualifier_rev; conflicts = Qualified_set.empty }
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_alias _ -> scope_ctx
    | Typdef_record rfl ->
      let scope_ctx = { scope_ctx with prefix_qualifier_rev = td.typdef_tconstr :: scope_ctx.prefix_qualifier_rev; conflicts = Qualified_set.empty } in
      (* order of declaration does not matter for class members:
         this is equivalent to implicit predefinitions. *)
      List.fold_left (fun scope_ctx (rf, _) ->
        begin match rf with
        | Record_field_member _ -> scope_ctx
        | Record_field_method rfm ->
          let error = "C_scope.enter_scope: expected field method" in
          let (v, _, _, _) = trm_inv ~error trm_let_fun_inv rfm in
          assert (v.qualifier = []);
          fst (map_binder scope_ctx v (Some (Trm.fun_with_empty_body rfm)))
        end
      ) scope_ctx rfl
    | _ -> failwith "unexpected typdef_body"
    end
  | _ ->
    { scope_ctx with conflicts = Qualified_set.empty }

(** internal *)
let scope_ctx_exit_namespace name outer_ctx inner_ctx =
  (* DEPRECATED: namespace prefix is already added on bind.
  let update_qualifier (q, n) = (name :: q, n) in
  let may_update_qualifier ((qname, id): Qualified_name.t * var_id) =
    if (Qualified_map.find_opt qname outer_ctx.var_ids) = (Some id)
    then (qname, id) (* this variable was the same in the outer scope *)
    else (update_qualifier qname, id)
  in *)
  let is_qualified ((q, n): typconstr) : bool =
    match (q, inner_ctx.prefix_qualifier_rev) with
    | (fq :: _, pq :: _) -> fq = pq
    | _ ->
      false
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
    conflicts = Qualified_set.union outer_ctx.conflicts (Qualified_set.filter is_qualified (inner_ctx.conflicts));
    predefined = Qualified_set.union outer_ctx.predefined (Qualified_set.filter is_qualified (inner_ctx.predefined));
    var_ids = Qualified_map.union union_var_ids outer_ctx.var_ids (Qualified_map.filter (fun k v -> is_qualified k) inner_ctx.var_ids) }

(** internal *)
let scope_ctx_exit_typedef td =
  scope_ctx_exit_namespace td.typdef_tconstr

(** Given term [t], check that all variable ids agree with their qualified name for C/C++ scoping rules. *)
let check_var_ids (t : trm) : unit =
  ignore (trm_map_vars ~keep_ctx: true ~enter_scope:(enter_scope check_map_binder) ~exit_typedef:scope_ctx_exit_typedef ~exit_namespace:scope_ctx_exit_namespace ~map_binder:check_map_binder check_map_var (toplevel_scope_ctx ()) t)

(** internal *)
let infer_map_binder (scope_ctx : scope_ctx) var t =
  let var' = if var.id = -1
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
  if var.id = -1 then
    trm_var ~annot ?loc ?typ ~ctx ~kind begin match Qualified_map.find_opt qualified scope_ctx.var_ids with
    | Some id -> { qualifier = var.qualifier; name = var.name; id }
    | None -> toplevel_free_var ~qualifier:var.qualifier var.name
    end
  else check_map_var scope_ctx (annot, loc, typ, ctx, kind) var

(** Given term [t], infer variable ids such that they agree with their qualified name for C/C++ scoping rules.
  Only variable ids equal to [-1] are inferred, other ids are checked. *)
let infer_var_ids (t : trm) : trm =
  if debug then Xfile.put_contents "/tmp/ids_before.txt" (Ast_to_text.ast_to_string t);
  let t2 = trm_map_vars ~keep_ctx:true ~enter_scope:(enter_scope infer_map_binder) ~exit_typedef:scope_ctx_exit_typedef ~exit_namespace:scope_ctx_exit_namespace ~map_binder:infer_map_binder infer_map_var (toplevel_scope_ctx ()) t in
  if debug then Xfile.put_contents "/tmp/ids_after.txt" (Ast_to_text.ast_to_string t2);
  t2
