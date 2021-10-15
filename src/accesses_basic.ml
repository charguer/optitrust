open Ast


(* [transform f_get f_set]: expects the target [tg] to point to a set operation or a get operation 
    if is a get operation then f_get will be applied on the node represented by target [tg]. Otherwise
    it is a set oepration then f_set will be applied on the second argument of the targeted node
*)
let transform (f_get : trm -> trm) (f_set : trm -> trm) : Target.Transfo.t = 
  Target.apply_on_targets (Accesses_core.transform f_get  f_set) 

(* [scale arg tg] this function is a specialization of the function transform where the functions f_get and f_set 
    are given explicitly as the division and multiplication operations respectively
*)
let scale (arg : trm) (tg : Target.target) : unit = 
  let f_get t = Arith_core.apply_aux Binop_div arg t in
  let f_set t = Arith_core.apply_aux Binop_mul arg t in
  transform f_get f_set tg


(* [shift arg tg] this function is a specialization of the function transform where the functions f_get and f_set 
    are given explicitly as the substraction  and addition respectively
*)
let shift (arg : trm) (tg : Target.target) : unit = 
  let f_get t = Arith_core.apply_aux Binop_sub arg t in
  let f_set t = Arith_core.apply_aux Binop_add arg t in
  transform f_get f_set tg
 