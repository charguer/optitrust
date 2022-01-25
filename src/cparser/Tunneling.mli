open AST
open Coqlib
open Datatypes
open LTL
open List0
open Maps
open UnionFind

module U :
 sig
  type elt = PTree.elt

  val elt_eq : elt -> elt -> bool

  type t

  val repr : t -> elt -> elt

  val empty : t

  val find : t -> elt -> elt * t

  val union : t -> elt -> elt -> t

  val merge : t -> elt -> elt -> t

  val pathlen : t -> elt -> nat
 end

val record_branch : U.t -> node -> bblock -> U.t

val record_branches : coq_function -> U.t

val record_cond :
  ((code * U.t) * bool) -> node -> bblock -> (code * U.t) * bool

val record_conds_1 : (code * U.t) -> (code * U.t) * bool

val record_conds : (code * U.t) -> U.t

val record_gotos : coq_function -> U.t

val tunnel_instr : U.t -> instruction -> instruction

val tunnel_block : U.t -> bblock -> bblock

val tunnel_function : coq_function -> coq_function

val tunnel_fundef : fundef -> fundef

val tunnel_program : program -> program
