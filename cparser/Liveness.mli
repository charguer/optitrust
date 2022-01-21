open AST
open BinNums
open Datatypes
open Kildall
open Lattice
open List0
open Maps
open RTL
open Registers

val reg_option_live : reg option -> Regset.t -> Regset.t

val reg_sum_live : (reg, ident) sum -> Regset.t -> Regset.t

val reg_list_live : reg list -> Regset.t -> Regset.t

val reg_list_dead : reg list -> Regset.t -> Regset.t

val transfer : coq_function -> node -> Regset.t -> Regset.t

module RegsetLat :
 sig
  type t = Regset.t

  val beq : t -> t -> bool

  val bot : t

  val lub : t -> t -> t
 end

module DS :
 sig
  module L :
   SEMILATTICE

  val fixpoint :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) -> L.t
    PMap.t option

  val fixpoint_allnodes :
    'a1 PTree.t -> ('a1 -> positive list) -> (positive -> L.t -> L.t) -> L.t
    PMap.t option
 end

val analyze : coq_function -> Regset.t PMap.t option

val last_uses_at : Regset.t PMap.t -> node -> instruction -> reg list

val last_uses : coq_function -> reg list PTree.t
