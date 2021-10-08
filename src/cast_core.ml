open Ast


(* [isnert_aux ty t] add a type in front of [t]
    to cast its current type to [ty]
*)
let insert_aux (ty : typ) (t : trm) : trm =
  trm_cast ty t

let insert (ty : typ) : Target.Transfo.local =
  Target.apply_on_path (insert_aux ty)


