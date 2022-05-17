open Ast

(* [add_aux label t]: adds label [label] to trm [t]
      [label] - label that is going to label trm [t]
      [t] - a trm *)
let add_aux (label : string) (t : trm) : trm =
  trm_labelled label t

(* [add_label label t p]: applies [add_aux] at the trm [t] with path [p] *)
let add (label : string) : Target.Transfo.local =
  Target.apply_on_path (add_aux label)

(* [remove_aux]: converts the labelled trm [t] to a simple trm
      [t] - ast node of the labelled trm *)
let remove_aux (t : trm) : trm =
  match t.desc with
  | Trm_labelled (_, tbody) -> trm_pass_pragmas t tbody
  | _ -> fail t.loc "Label_core.label_rem_aux: label was not matched, make sure the path is correct"

(* [remove t p]: applies [remove_aux] at trm [t] with path [p]. *)
let remove : Target.Transfo.local =
  Target.apply_on_path (remove_aux)
