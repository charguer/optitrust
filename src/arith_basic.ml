open Ast
include Arith_core


(* [shift ~neg ~pre_cast ~post_cast u] *)
let shift ?(reparse : bool = false) ?(neg : bool = false) ?(pre_cast : typ option) ?(post_cast : typ option) (u : trm) : Target.Transfo.t =
  Target.reparse_after ~reparse (
    Target.apply_on_targets (Arith_core.shift neg pre_cast post_cast u ) )

(* [apply op arg] expects the target [tg] to point to any node of the ast
      then it applies the binary operation [op] at that node with the second argument
      of that operation being [arg]
 *)
let apply (op : binary_op) (arg : trm) : Target.Transfo.t =
  Target.apply_on_targets (Arith_core.apply op arg)

let simpl (f: (expr -> expr)) : Target.Transfo.t =
  Target.apply_on_targets (Arith_core.simplify f)

let simplify : Target.Transfo.t =
  simpl Arith_core.gather_rec

(*
let normalize : Target.Transfo.t =
  simpl Arith_core.normalize
*)