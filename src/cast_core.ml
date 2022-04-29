open Ast


(* [insert_aux ty t]: add a type in front of [t] to cast its current type to [ty]
    params:
      [ty]:the type on which [t] is going to be casted to
      [t]: any ast node that allows casting *)
let insert_aux (ty : typ) (t : trm) : trm =
  trm_cast ty t

(* [insert ty t p]: apply [insert_aux] at term [t] with path [p] *)
let insert (ty : typ) : Target.Transfo.local =
  Target.apply_on_path (insert_aux ty)


