open Ast

(* [shift ~neg ~pre_cast ~post_cast u] expects the target [tg] to point 
    to a set operation of a get operation then it will replace trm with an 
    operation which can be an addition or substraction of that trm and [code].
    [neg] is a flag to decide if the new trm is an addition or a substraction
    [pre_cast] is a typ option, by default is None if its given than the targeted
    trm will be first casted before applying the operation.
    [post_cast] similat to [pre_cast] but the casting is done after applying the 
    operation.
*)
let shift ?(neg : bool = false) ?(pre_cast : typ option) ?(post_cast : typ option) (code : trm) : Target.Transfo.t =
  Target.reparse_after (Target.apply_on_targets (Arith_core.shift neg pre_cast post_cast code ))

(* [apply op arg] expects the target [tg] to point to any node of the ast
      then it applies the binary operation [op] at that node with the second argument 
      of that operation being [arg]
 *)
let apply (op : binary_op) (arg : trm) : Target.Transfo.t = 
  Target.apply_on_targets (Arith_core.apply op arg)