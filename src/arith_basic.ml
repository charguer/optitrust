open Ast
open Target
include Arith_core

(* [shift ~reparse ~inv ~pre_cast ~post_cast u tg]:  expects the target [tg]
    to point at a trm on which an arithmetic operation can be applied, then
    depending on the value of [inv] it will add or substract [u] to that trm.*)
let shift ?(reparse : bool = false) ?(inv : bool = false) ?(pre_cast : typ option)
  ?(post_cast : typ option) (u : trm) : Transfo.t =
  reparse_after ~reparse (
    apply_on_targets (Arith_core.transform Arith_shift inv u pre_cast post_cast) )

(* [scale ~inv ~pre_cast ~post_cast u] *)
let scale ?(reparse : bool = false) ?(inv : bool = false) ?(pre_cast : typ option)
  ?(post_cast : typ option) (u : trm) : Transfo.t =
  reparse_after ~reparse (
    apply_on_targets (Arith_core.transform Arith_scale inv u pre_cast post_cast) )

(* [apply op arg] expects the target [tg] to be pointing at any node of the ast
      then it applies the binary operation [op] at that node with the second argument
      of that operation being [arg] *)
let apply (op : binary_op) (arg : trm) : Transfo.t =
  apply_on_targets (Arith_core.apply op arg)

(* [simpl f] applies a arithmetic rewriting method from the module Arith_core:
   - gather  for grouping and cancelling out similar expressions in sums and produts
   - expand  for expanding products involving sums. *)
let simpl ?(indepth : bool = false) (f: (expr -> expr)) : Transfo.t =
  apply_on_targets (Arith_core.simplify indepth f)

(* [simpl_rec f tg] just an alias for simpl ~indepth:true tg *)
let simpl_rec (f : (expr -> expr)) : Transfo.t =
  simpl ~indepth:true f


(* [simplify ~indepth tg] applies simpl with the operation being gathering of
    arithmetic experssions *)
let simplify ?(indepth : bool = false) : Transfo.t =
  simpl ~indepth Arith_core.gather_rec

(* alias cPrimArith *)
let constr =
  cPrimPredFun is_prim_arith

(* [clear_nosimpl tg]: clears all the marks on all the instructions that where
    skipped by the simplifier *)
let clear_nosimpl (tg : target) : unit =
  Marks.remove Arith_core.mark_nosimpl [nbMulti; cMark Arith_core.mark_nosimpl]

(* [nosimplf tg]: mark all the instructions targeted by [tg] as "__arith_core_nosimpl" *)
let nosimpl (tg : target) : unit =
  Marks.add Arith_core.mark_nosimpl tg

(* LATER: have a stack of different marks to avoid loosing the previously existing ones *)

(* [with_nosimpl tg f]: after marking all the nodes targeted by [tg] with mark "__arith_core_with_nosimpl", applies the
    transformation [f] on all the nodes matched  by [tg], after the transformation has been applied succesfully,
    it will clean all the introduced marks *)
let with_nosimpl (tg : target) (f : unit -> unit) : unit =
  nosimpl tg;
  f();
  clear_nosimpl tg
