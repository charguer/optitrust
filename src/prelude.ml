include Ast
include Trm
include Typ
include Mark
include Target

let trm_seq_nobrace = Nobrace.trm_seq
let trm_seq_nobrace_nomarks = Nobrace.trm_seq_nomarks

let skip_includes (t : trm) : trm =
  match trm_seq_inv t with
  | Some instrs ->
    let _, not_include = Mlist.partition trm_is_include instrs in
    trm_seq not_include
  | None -> t

(* TODO: reflect on the API implications of #var-id (e.g. where this function is called) *)
let find_var_in_current_ast ?(target : target = []) (name : string) : var =
  let t = Trace.ast () in
  let vars =
    if target = [] then trm_def_or_used_vars (skip_includes t)
    else List.fold_left (fun acc p ->
      Var_set.union acc (trm_def_or_used_vars (Path.resolve_path p t))
    ) Var_set.empty (resolve_target target t)
  in
  let candidates = Var_set.filter (fun v -> v.qualifier = [] && v.name = name) vars in
  match Var_set.cardinal candidates with
  | 0 -> failwith (sprintf "could not find variable '%s' in current AST variables: %s" name (vars_to_string (Var_set.elements vars)))
  | 1 -> Var_set.choose candidates
  | n -> failwith (sprintf "%d variables with name '%s' found in current AST: %s" n name (vars_to_string (Var_set.elements candidates)))

(* [AstParser]: module for integrating pieces of code given as input by the user. *)
module AstParser = struct
  let var v = trm_var (name_to_var v)
  (* DEPRECATED: (find_var_in_current_ast v) *)

  let var_mut v = trm_var_get (name_to_var v)
  (* DEPRECATED: (find_var_in_current_ast v) *)

  let lit l = code (Lit l)

  let ty ty = typ_str (Atyp ty)

  let subst_dollar_number (inst : var list) (s : string) : string =
    failwith "#var-id"
  (* Xlist.fold_lefti (fun i acc insti ->
    Tools.string_subst ("${"^(string_of_int i) ^ "}") insti acc
  ) s inst
*)
  let expr ?(vars : var list = []) (e : string)  =
    let e = if vars = [] then e else subst_dollar_number vars e in
    code (Expr e)

  let stmt s = code (Stmt s)

  let instr s = code (Instr s)

end
include AstParser
