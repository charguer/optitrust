open Ast
open Target
open Transformations

(* label_add: This function is an auxiliary function for label_add
    params: 
      path_to_instr: path to the instruction which is going to be labeled after
      label: a string representing the label to be added
      t: ast 
    return:
      the updated ast
 *)

 let label_add (path_to_instr : path) (label : string) (t : trm) : trm =
  apply_local_transformation(trm_labelled ("label " ^ label)) t path_to_instr

(* label_rem_aux: This function is an auxiliary function for label_rem
    params:
      subt: an ast subter
    return:
      the updated ast   
*)
let label_rem_aux (subt : trm) : trm =
  match subt.desc with 
  | Trm_labelled (_, t) -> t
  | _ -> fail subt.loc "label_rem_aux: label was not matched, make sure the path is correct"


(* label_rem: This function is an auxiliary function for label_rem
    params: 
      path_to_label: path to the instruction which is going to be labeled after
      t: ast 
    return:
      the updated ast
 *)
let label_rem (path_to_label : path) (t : trm) : trm = 
  apply_local_transformation(label_rem_aux ) t path_to_label

  