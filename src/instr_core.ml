open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [replace code t]: replace an instruction with arbitrary code
    params:
      [cd]: string representing the code which will appear in place of the targeted trm
      [t]: ast of the trm going to be replaced
    return:
      updated ast with the replaced trm
 *)
let replace (cd : string) : Target.Transfo.local =
  Target.apply_on_path (fun _t -> code cd)




(* [replace_fun name t]: change the current function call
      to another function call with where the function called now
      has name [name]
    params:
      [name]: name of the function replacing the targeted one
      [t]: ast of the function call trm
    return:
      updated ast with the replaced trm
 *)

let replace_fun_aux (name : string) (t : trm) : trm =
  match t.desc with
  | Trm_apps (_, args) ->
    trm_apps ~annot:t.annot ~marks:t.marks ~typ:t.typ (trm_var name) args
  | _ -> fail t.loc "replace_fun: expected a function call"


let replace_fun (name : string) : Target.Transfo.local =
  Target.apply_on_path (replace_fun_aux name)

(* [move_aux index tg_index t]: move instruction at [index] to [index_instr]
    in the sequence [t]
    params:
      [index]: index where the instr should be move to
      [tg_index]: the current index of the targeted instruction
    return:
      the updated [t]
*)
let move_aux (index : int) (tg_index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let instr_to_move = Mlist.nth tl tg_index in
    let instr_to_move = {instr_to_move with marks = []} in
    let new_tl = Mlist.insert_at index instr_to_move tl in
    trm_seq ~annot:t.annot ~marks:t.marks new_tl
  | _ -> fail t.loc "move_aux: expected the sequence which contains the surrounding sequence"

let move (index : int) (tg_index : int) : Target.Transfo.local =
  Target.apply_on_path (move_aux index tg_index)
