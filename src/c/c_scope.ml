open Ast
open Trm

let debug = false

(* FIXME: triggers on multiple function declarations/definitions. *)
exception InvalidVarId of string

(** Set of variables defined in the current surrounding sequence,
    Set of variables pre-defined in the current scope,
    Map from variables to their unique ids. *)
type scope_ctx = {
  conflicts: Qualified_set.t;
  predefined: Qualified_set.t;
  var_ids: var_id Qualified_map.t;
}

(** internal *)
let clear_conflicts scope_ctx =
  { scope_ctx with conflicts = Qualified_set.empty }

(** internal *)
let toplevel_scope_ctx (): scope_ctx = {
  conflicts = Qualified_set.empty;
  predefined = Qualified_set.empty; (* FIMXE: could add some toplevel vars that we allow redefining here. *)
  var_ids = !toplevel_free_vars;
}

(** internal *)
let check_map_var (scope_ctx : scope_ctx) (annot, loc, typ, ctx, kind) var =
  let qualified = (var.qualifier, var.name) in
  if Qualified_map.find_opt qualified scope_ctx.var_ids <> Some var.id then
    raise (InvalidVarId (sprintf "variable use %s does not satisfy C/C++ scoping rules" (var_to_string var)))
  else
    trm_var ~annot ?loc ?typ ~ctx ~kind var

(** internal *)
let check_map_binder (scope_ctx : scope_ctx) var t =
  let qualified = (var.qualifier, var.name) in
  ((if var.name = "" then
    (* C/C++ allows giving no variable names *)
    scope_ctx
  else if Qualified_set.mem qualified scope_ctx.conflicts then
    raise (InvalidVarId (sprintf "redefinition of variable '%s' is illegal in C/C++" (var_to_string var)))
  else if (Option.map Trm.is_fun_with_empty_body t) = (Some true) then
    (* predeclarations don't conflict *)
    { scope_ctx with predefined = Qualified_set.add qualified scope_ctx.predefined;
     var_ids = Qualified_map.add qualified var.id scope_ctx.var_ids }
  else
    { scope_ctx with conflicts = Qualified_set.add qualified scope_ctx.conflicts;
      var_ids = Qualified_map.add qualified var.id scope_ctx.var_ids }
  ), var)

(** internal *)
let scope_ctx_exit_namespace name outer_ctx inner_ctx =
  let update_qualifier (q, n) = (name :: q, n) in
  let may_update_qualifier ((qname, id): Qualified_name.t * var_id) =
    if (Qualified_map.find_opt qname outer_ctx.var_ids) = (Some id)
    then (qname, id) (* this variable was the same in the outer scope *)
    else (update_qualifier qname, id)
  in
  { conflicts = Qualified_set.union outer_ctx.conflicts (Qualified_set.map update_qualifier (inner_ctx.conflicts));
    predefined = Qualified_set.union outer_ctx.predefined (Qualified_set.map update_qualifier (inner_ctx.predefined));
    var_ids = Qualified_map.of_seq (Seq.map may_update_qualifier (Qualified_map.to_seq inner_ctx.var_ids)) }

(** internal *)
let scope_ctx_exit_typedef td =
  scope_ctx_exit_namespace td.typdef_tconstr

(** Given term [t], check that all variable ids agree with their qualified name for C/C++ scoping rules. *)
let check_var_ids (t : trm) : unit =
  ignore (trm_map_vars ~keep_ctx: true ~enter_scope:clear_conflicts ~exit_typedef:scope_ctx_exit_typedef ~exit_namespace:scope_ctx_exit_namespace ~map_binder:check_map_binder check_map_var (toplevel_scope_ctx ()) t)

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
  (* printf "vars: %s" (Var_set.elements) *)
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
  let t2 = trm_map_vars ~keep_ctx:true ~enter_scope:clear_conflicts ~exit_typedef:scope_ctx_exit_typedef ~exit_namespace:scope_ctx_exit_namespace ~map_binder:infer_map_binder infer_map_var (toplevel_scope_ctx ()) t in
  if debug then Xfile.put_contents "/tmp/ids_after.txt" (Ast_to_text.ast_to_string t2);
  t2
