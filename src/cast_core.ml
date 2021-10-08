open Ast


let insert_aux (ty : typ) (t : trm) : trm =
  trm_cast ty t

let insert (ty : typ) : Target.Transfo.local =
  Target.apply_on_path (insert_aux ty)


