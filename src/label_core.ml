open Ast

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [add_aux label t]: add a lable around term t
    params:
      label: a string representing the label 
      t: an ast node
    return:
      the updated ast with new labelled term *)
let add_aux (label : string) (t : trm) : trm =
  trm_labelled label t

let add (label : string) : Target.Transfo.local =
  Target.apply_on_path (add_aux label)

(* remove_aux: remove the label from a labelled trm
    params:
      t: ast node of the labelled t
    return:
      the updated ast *)
let remove_aux (t : trm) : trm =
  match t.desc with
  | Trm_labelled (_, tbody) -> tbody
  | _ -> fail t.loc "label_rem_aux: label was not matched, make sure the path is correct"

let remove : Target.Transfo.local =
  Target.apply_on_path (remove_aux)

