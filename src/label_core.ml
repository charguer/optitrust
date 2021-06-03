open Ast

(* add_aux : The function is an auxiliary function for add
    params:
      t: an ast subterm
    return:
      the updated ast
*)
let add_aux (label : string) (t : trm) : trm = 
  trm_labelled label t


(* [add label t p] : label a targeted ast trm
    params:
      t: ast
      path_to_instr: path to the instruction we want to label
    return: 
      the updated ast 
*)
let add (label : string) : Target.Transfo.local =
  Target.apply_on_path (add_aux label)

(* remove_aux: This function is an auxiliary function for label_rem
    params:
      t: an ast subterm
    return:
      the updated ast
*)
let remove_aux (t : trm) : trm =
  match t.desc with
  | Trm_labelled (_, tbody) -> tbody
  | _ -> fail t.loc "label_rem_aux: label was not matched, make sure the path is correct"


(* remove: extract the trm inside the labelled trm
    params:
      path_to_label: path to the instruction which is going to be labeled after
      t: ast
    return:
      the updated ast
 *)
let remove : Target.Transfo.local =
  Target.apply_on_path (remove_aux)

