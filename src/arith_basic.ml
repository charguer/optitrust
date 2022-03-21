open Ast
include Arith_core


(* [shift ~neg ~pre_cast ~post_cast u] *)
let shift ?(reparse : bool = false) ?(neg : bool = false) ?(pre_cast : typ option) ?(post_cast : typ option) (u : trm) : Target.Transfo.t =
  Target.reparse_after ~reparse (
    Target.apply_on_targets (Arith_core.transform Arith_shift neg pre_cast post_cast u ) )

(* [scale ~neg ~pre_cast ~post_cast u] *)
let scale ?(reparse : bool = false) ?(inv : bool = false) ?(pre_cast : typ option) ?(post_cast : typ option) (u : trm) : Target.Transfo.t =
  Target.reparse_after ~reparse (
    Target.apply_on_targets (Arith_core.transform Arith_scale inv pre_cast post_cast u ) )

(* [apply op arg] expects the target [tg] to point to any node of the ast
      then it applies the binary operation [op] at that node with the second argument
      of that operation being [arg]
 *)
let apply (op : binary_op) (arg : trm) : Target.Transfo.t =
  Target.apply_on_targets (Arith_core.apply op arg)

(* [simpl f] applies a arithmetic rewriting method from the module Arith_core:
   - gather  for grouping and cancelling out similar expressions in sums and produts
   - expand  for expanding products involving sums. *)
let simpl ?(indepth : bool = false) (f: (expr -> expr)) : Target.Transfo.t =
  Target.apply_on_targets (Arith_core.simplify indepth f)

(* [simpl_rec f tg] just an alias for simpl ~indepth:true tg *)
let simpl_rec (f : (expr -> expr)) : Target.Transfo.t =
  simpl ~indepth:true f

let simplify ?(indepth : bool = false) : Target.Transfo.t =
  simpl ~indepth Arith_core.gather_rec

(*
let normalize : Target.Transfo.t =
  simpl Arith_core.normalize
*)

let is_prim_arith (p : prim) : bool =
  match p with
  | Prim_binop (Binop_add | Binop_sub | Binop_mul | Binop_div)
  | Prim_unop Unop_neg ->
      true
  | _ -> false

let constr = (* alias cPrimArith *)
  Target.cPrimPredFun is_prim_arith
