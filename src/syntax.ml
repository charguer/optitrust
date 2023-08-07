include Ast
include Trm
include Typ
include Mark

let trm_seq_no_brace = Nobrace.trm_seq

(* [AstParser]: module for integrating pieces of code given as input by the user. *)
module AstParser = struct
  let var v = trm_var v

  let var_mut v = trm_var_get v

  let lit l = code (Lit l)

  let ty ty = typ_str (Atyp ty)

  let subst_dollar_number (inst : string list) (s : string) : string =
  Xlist.fold_lefti (fun i acc insti ->
    Tools.string_subst ("${"^(string_of_int i) ^ "}") insti acc
  ) s inst

  let expr ?(vars : var list = []) (e : string)  =
    let e = if vars = [] then e else subst_dollar_number vars e in
    code (Expr e)

  let stmt s = code (Stmt s)

  let instr s = code (Instr s)

end