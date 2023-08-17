open Ast
open Trm

exception InvalidVarId of string

type var_ids = var_id Qualified_map.t

(** internal *)
let check_map_binder (var_ids : var_ids) var =
  let qualified = (var.qualifier, var.name) in
  if Qualified_map.mem qualified var_ids then raise (InvalidVarId (sprintf "variable binding %s does not satisfy C/C++ scoping rules" (var_to_string var)))
  else (Qualified_map.add qualified var.id var_ids, var)

(** internal *)
let check_map_var (var_ids : var_ids) (annot, loc, typ, ctx) var =
  let qualified = (var.qualifier, var.name) in
  if Qualified_map.find_opt qualified var_ids <> Some var.id then raise (InvalidVarId (sprintf "variable use %s does not satisfy C/C++ scoping rules" (var_to_string var)))
  else trm_var ~annot ?loc ?typ ~ctx var

(** Given term [t], check that all variable ids agree with their qualified name for C/C++ scoping rules. *)
let check_var_ids (t : trm) : unit =
  ignore (trm_map_vars check_map_binder check_map_var Qualified_map.empty t)

(** internal *)
let infer_map_binder (var_ids : var_ids) var =
  let var' = if var.id = -1
    then new_var ~qualifier:var.qualifier var.name
    else var
  in
  check_map_binder var_ids var'

(** internal *)
let infer_map_var (var_ids : var_ids) (annot, loc, typ, ctx) var =
  let qualified = (var.qualifier, var.name) in
  (* printf "vars: %s" (Var_set.elements) *)
  if var.id = -1 then
    trm_var ~annot ?loc ?typ ~ctx begin match Qualified_map.find_opt qualified var_ids with
    | Some id -> { qualifier = var.qualifier; name = var.name; id }
    | None -> toplevel_var ~qualifier:var.qualifier var.name
    end
  else check_map_var var_ids (annot, loc, typ, ctx) var

(** Given term [t], infer variable ids such that they agree with their qualified name for C/C++ scoping rules.
  Only variable ids equal to [-1] are inferred, other ids are only checked. *)
let infer_var_ids (t : trm) : trm =
  let t2 = trm_map_vars ~keep_ctx:true infer_map_binder infer_map_var Qualified_map.empty t in
  Xfile.put_contents "/tmp/a_enc.txt" (Ast_to_text.ast_to_string t);
  Xfile.put_contents "/tmp/b_enc.txt" (Ast_to_text.ast_to_string t2);
  t2
