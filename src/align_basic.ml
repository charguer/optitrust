open Ast
open Target


(* [def vec_align tg] expects the target [tg] to be pointing at an array declaration, then it will add the alignas attribute to its type
     with [vec_align] *)
let def (vec_align : trm) : Target.Transfo.t = 
  Target.apply_on_targets(Align_core.def vec_align)
