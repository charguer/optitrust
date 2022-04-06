open Ast

(* [transform f_get f_set]: expects the target [tg] to point to a set operation or a get operation
    if is a get operation then f_get will be applied on the node represented by target [tg]. Otherwise
    it is a set oepration then f_set will be applied on the second argument of the targeted node
*)
let transform ?(reparse : bool = false) (f_get : trm -> trm) (f_set : trm -> trm) : Target.Transfo.t =
   Target.reparse_after ~reparse (
     Target.apply_on_targets (fun t p ->
        let get_or_set_path = Internal.get_surrounding_trm (fun t -> (is_get_operation t) || (is_set_operation t)) p t in
        if get_or_set_path = [] then t else
        Accesses_core.transform f_get  f_set t get_or_set_path ))

(* [scale ~factor ~factor_ast tg] this transformation is an andvanced version of Accesses_basic.scale
     This transformation expects the target [tg] to be pointing at a node that is na ancestor of a get or
     set operation *)
let scale ?(inv:bool=false) ?(reparse : bool = false) ~factor:(factor:trm) (tg : Target.target) : unit =
  let op_get, op_set = if inv then (Binop_mul, Binop_div) else (Binop_div, Binop_mul) in
  let f_get t = Arith_core.apply_aux op_get factor t in
  let f_set t = Arith_core.apply_aux op_set factor t in
  transform ~reparse f_get f_set tg
(* LATER: factorize with basic *)

(* [shift ~factor ~factor_ast tg] this function is a specialization of the function transform where the functions f_get and f_set
    are given explicitly as the substraction  and addition respectively
*)
let shift ?(inv:bool=false) ?(reparse : bool = false) ~factor:(factor : trm) (tg : Target.target) : unit =
  let op_get, op_set = if inv then (Binop_add, Binop_sub) else (Binop_sub, Binop_add) in
  let f_get t = Arith_core.apply_aux op_get factor t in
  let f_set t = Arith_core.apply_aux op_set factor t in
  transform ~reparse f_get f_set tg
 (* LATER: Define shift_access that applies to a target on accesses and calls shift on the parent path *)


(* [intro tg] expects the target [tg] to be pointing at any node which could contain struct accesses, preferably a sequence
    then it will replace all the nodes with encoding as struct_get (get (t), f) -> get (struct_access (t, f))
*)
let intro : Target.Transfo.t =
  Target.apply_on_targets (Accesses_core.intro )
