open Ast

let insert (ty : typ) : Target.Transfo.t =
  Target.apply_on_targets (Cast_core.insert ty)


  