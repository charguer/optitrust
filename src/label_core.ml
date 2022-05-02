open Ast

(* [add_aux label t]: add label [label] to trm [t]
    params:
      label: label that is going to label trm [t]
      t: a trm *)
let add_aux (label : string) (t : trm) : trm =
  trm_labelled label t

(* [add_label label t p]: apply [add_aux] at the trm [t] with path [p] *)
let add (label : string) : Target.Transfo.local =
  Target.apply_on_path (add_aux label)

(* remove_aux: convert the labelled trm [t] to a simple trm
    params:
      t: ast node of the labelled trm *)
let remove_aux (t : trm) : trm =
  match t.desc with
  | Trm_labelled (_, tbody) -> tbody
  | _ -> fail t.loc "Label_core.label_rem_aux: label was not matched, make sure the path is correct"

let remove : Target.Transfo.local =
  Target.apply_on_path (remove_aux)
