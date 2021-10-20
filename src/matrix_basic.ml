
open Ast


let intro_mcalloc : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.intro_mcalloc)


let intro_mmalloc : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.intro_mmalloc)

let intro_mindex (dims : trms) : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.intro_mindex dims)

let reorder_dims (order : int list) : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.reorder_dims order)