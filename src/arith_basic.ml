open Ast
include Arith_core


(* [shift ~inv ~pre_cast ~post_cast u] *)
let shift ?(reparse : bool = false) ?(inv : bool = false) ?(pre_cast : typ option) ?(post_cast : typ option) (u : trm) : Target.Transfo.t =
  Target.reparse_after ~reparse (
    Target.apply_on_targets (Arith_core.transform Arith_shift inv pre_cast post_cast u ) )

(* [scale ~inv ~pre_cast ~post_cast u] *)
let scale ?(reparse : bool = false) ?(inv : bool = false) ?(pre_cast : typ option) ?(post_cast : typ option) (u : trm) : Target.Transfo.t =
  Target.reparse_after ~reparse (
    Target.apply_on_targets (Arith_core.transform Arith_scale inv pre_cast post_cast u ) )

(* [apply op arg] expects the target [tg] to be pointing at any node of the ast
      then it applies the binary operation [op] at that node with the second argument
      of that operation being [arg] *)
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


(* [simplify ~indepth tg] applies simpl with the operation being gathering of arithmetic experssions *)
let simplify ?(indepth : bool = false) : Target.Transfo.t =
  simpl ~indepth Arith_core.gather_rec

(* alias cPrimArith *)
let constr = 
  Target.cPrimPredFun is_prim_arith

(* [clear_nosimpl tg] clearn all the marks on all the instructions that where skipped by the simplifier *)
let clear_nosimpl (tg : Target.target) : unit =
  Marks.remove Arith_core.mark_nosimpl [Target.nbMulti; Target.cMark Arith_core.mark_nosimpl]

(* [nosimplf tg] mark all the instructions targeted by [tg] as "__arith_core_nosimpl" *)
let nosimpl (tg : Target.target) : unit =
  Marks.add Arith_core.mark_nosimpl tg
  
(* LATER: have a stack of different marks to avoid loosing the previously existing ones *)

(* [with_nosimpl tg f] after marking all the nodes targeted by [tg] as "__arith_core_with_nosimpl" 
    apply transformation [f], then clean all the introduced marks *)
let with_nosimpl (tg : Target.target) (f : unit -> unit) : unit =
  nosimpl tg;
  f();
  clear_nosimpl tg

