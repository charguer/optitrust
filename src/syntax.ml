include Ast
include Trm
include Typ
include Mark

let trm_seq_no_brace = Nobrace.trm_seq

let find_var_in_current_ast (name : string) : var =
  let vars = trm_def_or_used_vars (Trace.ast ()) in
  let candidates = Var_set.filter (fun v -> v.qualifier = [] && v.name = name) vars in
  match Var_set.cardinal candidates with
  | 0 -> failwith (sprintf "could not find variable '%s' in current AST variables: %s" name (vars_to_string (Var_set.elements vars)))
  | 1 -> Var_set.choose candidates
  | n -> failwith (sprintf "%d variables with name '%s' found in current AST: %s" n name (vars_to_string (Var_set.elements candidates)))

(* [AstParser]: module for integrating pieces of code given as input by the user. *)
module AstParser = struct
  let var v = trm_var (find_var_in_current_ast v)

  let var_mut v = trm_var_get (find_var_in_current_ast v)

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