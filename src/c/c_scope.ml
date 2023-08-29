open Ast
open Trm

let debug = true

(* FIXME: triggers on multiple function declarations/definitions. *)
exception InvalidVarId of string

(** Set of variables defined in the current surrounding sequence, and maps from variables to their unique ids. *)
type scope_ctx = Qualified_set.t * var_id Qualified_map.t

(** internal *)
let clear_conflicts scope_ctx = (Qualified_set.empty, snd scope_ctx)

(** internal *)
let toplevel_scope_ctx (): scope_ctx = Qualified_set.empty, !toplevel_free_vars

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

(** internal *)
let scope_ctx_exit_typedef td old_ctx td_ctx =
  let class_name = td.typdef_tconstr in

(** internal *)
let scope_ctx_exit_namespace name outer_ctx inner_ctx =
  let may_update_qualifier ((qname, id): Qualified_name.t * var_id) =
    if (Qualified_map.find_opt qname (snd outer_ctx)) = (Some id)
    then (qname, id) (* this variable was the same in the outer scope *)
    else ((name :: fst qname, snd qname), id)
  in
  (fst outer_ctx, Qualified_map.of_seq (Seq.map may_update_qualifier (Qualified_map.to_seq (snd inner_ctx))))

(** Given term [t], check that all variable ids agree with their qualified name for C/C++ scoping rules. *)
let check_var_ids (t : trm) : unit =
  ignore (trm_map_vars ~keep_ctx: true ~enter_scope:clear_conflicts ~exit_typedef:scope_ctx_exit_typedef ~map_binder:check_map_binder check_map_var (toplevel_scope_ctx ()) t)

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
    | None -> toplevel_free_var ~qualifier:var.qualifier var.name
    end
  else check_map_var (conflict_set, var_ids) (annot, loc, typ, ctx, kind) var

(** Given term [t], infer variable ids such that they agree with their qualified name for C/C++ scoping rules.
  Only variable ids equal to [-1] are inferred, other ids are checked. *)
let infer_var_ids (t : trm) : trm =
  if debug then Xfile.put_contents "/tmp/ids_before.txt" (Ast_to_text.ast_to_string t);
  let t2 = trm_map_vars ~keep_ctx:true ~enter_scope:clear_conflicts ~exit_typedef:scope_ctx_exit_typedef ~map_binder:infer_map_binder infer_map_var (toplevel_scope_ctx ()) t in
  if debug then Xfile.put_contents "/tmp/ids_after.txt" (Ast_to_text.ast_to_string t2);
  t2
