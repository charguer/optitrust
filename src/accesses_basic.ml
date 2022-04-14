open Ast

(* [transform f_get f_set]: expects the target [tg] to point at a node inside a set or a get operation.
      Then the transformation will search for the first get or set operation and call the transform core transformation
      on that node.
      if is a get operation then f_get will be applied on the node represented by target [tg]. Otherwise
      it is a set oepration then f_set will be applied on the second argument of the targeted node. *)
let transform ?(reparse : bool = false) (f_get : trm -> trm) (f_set : trm -> trm) : Target.Transfo.t =
     Target.apply_on_targets (fun t p ->
        let get_or_set_path = Internal.get_surrounding_trm (fun t -> (is_get_operation t) || (is_set_operation t)) p t in
        if get_or_set_path = [] then t else
        Accesses_core.transform f_get  f_set t get_or_set_path )



(* [scale ~factor_ast tg] this transformation just calls transform with [f_get] and [f_set] args defined as 
     a multiplication and a division respectively. If [inv] is set to true then these two operations will be swapped. *)
let scale ?(inv:bool=false) ~factor:(factor:trm) (tg : Target.target) : unit =
  let op_get, op_set = if inv then (Binop_mul, Binop_div) else (Binop_div, Binop_mul) in
  let f_get t = Arith_core.apply_aux op_get factor t in
  let f_set t = Arith_core.apply_aux op_set factor t in
  transform f_get f_set tg


(* [shift ~factor_ast tg] this transformation just calls transform with [f_get] and [f_set] args defined as 
     a multiplication and a division respectively. If [inv] is set to true then these two operations will be swapped. *)
let shift ?(inv:bool=false) ~factor:(factor : trm) (tg : Target.target) : unit =
  let op_get, op_set = if inv then (Binop_add, Binop_sub) else (Binop_sub, Binop_add) in
  let f_get t = Arith_core.apply_aux op_get factor t in
  let f_set t = Arith_core.apply_aux op_set factor t in
  transform f_get f_set tg

(* [intro tg] expects the target [tg] to be pointing at any node which could contain struct accesses, preferably a sequence
    then it will replace all the nodes with encoding as struct_get (get (t), f) -> get (struct_access (t, f)) . *)
let intro : Target.Transfo.t =
  Target.apply_on_targets (Accesses_core.intro )
