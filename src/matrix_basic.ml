
open Ast

(* [intro_mcalloc tg] expects the target [tg] pointing to a call to funciton alloc  
      then it will replace this call with a call to MCALLOC
*)
let intro_mcalloc : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.intro_mcalloc)

(* [intro_mmalloc tg] expects the target [tg] pointing to a call to funciton malloc  
      then it will replace this call with a call to MMALLOC
*)
let intro_mmalloc : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.intro_mmalloc)

(* [intro_mindex dim tg] expects the target [tg] pointing to an array access 
    then it will replace that access to let say index i with an access at
    MINDEX (dim,i)
*)
let intro_mindex (dim : trm) : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.intro_mindex dim)

(* [reorder_dims order tg]: expects the target [tg] pointing to a call to ALLOC functions, or MINDEX 
      then it will reorder their args based on [order], where [order] is a list of indices which the 
      current args should follow
*)
let reorder_dims (order : int list) : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.reorder_dims order)

(* [redundant_dim new_dim]: expects the target [tg] pointing to call to ALLOC functions, then it will 
      add a new arg at the begining of the list of args in the targetd call
 *)
let redundeant_dim (new_dim : trm) : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.new_redundant_dim new_dim)