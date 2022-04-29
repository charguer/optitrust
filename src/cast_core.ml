open Ast


(* [insert_aux ty t]: add a type in front of [t] to cast its current type to [ty]
    params:
      [ty]:the type on which [t] is going to be casted to
      [t]: any ast node that allows casting *)
let insert_aux (ty : typ) (t : trm) : trm =
  trm_cast ty t

let insert (ty : typ) : Target.Transfo.local =
  Target.apply_on_path (insert_aux ty)


