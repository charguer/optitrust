open Ast
include Arith_core

(* [apply op arg] expects the target [tg] to point to any node of the ast
      then it applies the binary operation [op] at that node with the second argument
      of that operation being [arg]
 *)
let apply (op : binary_op) (arg : trm) : Target.Transfo.t =
  Target.apply_on_targets (Arith_core.apply op arg)



let simplify ?(f: (expr -> expr) = gather true) : Target.Transfo.t =
  Target.apply_on_targets (Arith_core.simplify f)