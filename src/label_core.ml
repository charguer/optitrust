open Ast

(* [add_aux label t]: adds label [label] to trm [t]
      [label] - label that is going to label trm [t]
      [t] - a trm *)
let add_aux (label : string) (t : trm) : trm =
  trm_add_label label t

(* [add_label label t p]: applies [add_aux] at the trm [t] with path [p] *)
let add (label : string) : Target.Transfo.local =
  Target.apply_on_path (add_aux label)

(* [remove_aux]: converts the labelled trm [t] to a simple trm
      [t] - ast node of the labelled trm *)
let remove_aux (t : trm) : trm =
  trm_rem_labels t

(* [remove t p]: applies [remove_aux] at trm [t] with path [p]. *)
let remove : Target.Transfo.local =
  Target.apply_on_path (remove_aux)
