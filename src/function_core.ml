open Ast
open Path

let bind_intro_aux (index : int) (fresh_name : var) (p_local : path) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     let lfront, lback = Tools.split_list_at index tl in
     let instr, lback = Tools.split_list_at 1 lback in
     let instr = List.hd instr in
     Tools.printf "trm : %s" (Ast_to_c.ast_to_string instr);
     let trm_to_apply_changes, _ = Path.resolve_path p_local instr in
     let decl_to_insert = trm_let Var_immutable (fresh_name, typ_auto()) trm_to_apply_changes in
     let decl_to_change = Generic_core.change_trm trm_to_apply_changes (trm_var fresh_name) instr in
     trm_seq ~annot:t.annot (lfront @ [decl_to_insert] @ [decl_to_change] @ lback)
  | _ -> fail t.loc "bind_intro_aux: expected the surrounding sequence"


let bind_intro (index : int) (fresh_name : var) (p_local : path) : Target.Transfo.local =
  Target.apply_on_path (bind_intro_aux index fresh_name p_local)

