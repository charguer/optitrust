open Ast

(* [label_add label t p]: This function is an auxiliary function for label_add
    params:
      label: a string representing the label to be added
      t: ast
      p: path to the instruction which is going to be labeled
    return:
      the updated ast
*)

 (* let label_add (path_to_instr : path) (label : string) (t : trm) : trm =
  apply_on_path (fun (subt : trm) -> trm_labelled label subt)
    t path_to_instr *)

(* TODO: THINK ABOUT
let label_add (label : string) (t : trm) (p : path) : trm =
  Target.apply_on_path (fun (subt : trm) -> trm_labelled label subt) t p

OR

let label_add_aux (label : string) (t : trm) : trm =
  trm_labelled label t

let label_add (label : string) (t : trm) (p : path) : trm =
  Target.apply_on_path (label_add_aux label) t p


CURRENT PROPOSAL:

--module Transfo    type local = trm -> path -> trm
  Target.local_transfo
  Target.Transfo.local

let add (label : string) : Target.Transfo.local =
  Target.apply_on_path (add_aux label)

*)

(* add_aux : The function is an auxiliary function for add
    params:
      t: an ast subterm
    return:
      the updated ast
*)
let add_aux (label : string) (t : trm) : trm = 
  trm_labelled label t


(* add : label a targeted ast trm
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

