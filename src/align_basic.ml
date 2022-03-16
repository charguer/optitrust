open Ast

let def (vec_align : trm) : Target.Transfo.t = 
  Target.apply_on_targets(Align_core.def vec_align)