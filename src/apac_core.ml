open Ast
open Target


(* [use_goto_for_return_aux mark t]: todo
    [mark] - mark used to mark the introduced sequence.
    [t] - ast of the function definition. *)
let use_goto_for_return_aux (mark : mark) (t : trm) : trm =
  match t.desc with 
  | Trm_let_fun (qn, ret_ty, args, body) -> 
    let seq_to_insert, _ = Internal.replace_return_with_assign ~check_terminal:false ~exit_label:"__exit" "__res" body in 
    let new_body = 
      begin match ret_ty.typ_desc with 
      | Typ_unit -> 
        trm_add_mark mark (trm_seq_nomarks [
          seq_to_insert;
          trm_add_label "__exit" (trm_unit())])
      | _ -> 
        let new_decl = trm_let_mut ("__res", ret_ty) (trm_uninitialized ()) in 
        trm_add_mark mark (trm_seq_nomarks [
          new_decl;
          seq_to_insert;
          trm_add_label "__exit" (trm_unit());
          trm_ret (Some (trm_var_get "__res"))
        ])
      end in 
      trm_alter ~desc:(Some (Trm_let_fun (qn, ret_ty, args, new_body))) t
  | _ -> fail t.loc "Apac_core.use_goto_for_return_aux: expected  a target to a function definition."


let use_goto_for_return (mark : mark) : Transfo.local =
  apply_on_path(use_goto_for_return_aux mark)