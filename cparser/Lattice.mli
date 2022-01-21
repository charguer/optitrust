open BinNums
open Bool
open FSetInterface
open Maps

module type SEMILATTICE =
 sig
  type t

  val beq : t -> t -> bool

  val bot : t

  val lub : t -> t -> t
 end

module type SEMILATTICE_WITH_TOP =
 sig
  type t

  val beq : t -> t -> bool

  val bot : t

  val lub : t -> t -> t

  val top : t
 end

module LPMap1 :
 functor (L:SEMILATTICE) ->
 sig
  type t = L.t PTree.t

  val get : positive -> t -> L.t

  val set : positive -> L.t -> t -> t

  val beq : t -> t -> bool

  val bot : t

  val opt_beq : L.t option -> L.t option -> bool

  type changed =
  | Same
  | Same1
  | Same2
  | Changed of L.t PTree.t

  val xcombine :
    (L.t option -> L.t option -> L.t option) -> L.t PTree.tree -> L.t
    PTree.tree -> changed

  val combine :
    (L.t option -> L.t option -> L.t option) -> L.t PTree.t -> L.t PTree.t ->
    L.t PTree.t

  val lub : t -> t -> t
 end

module LPMap :
 functor (L:SEMILATTICE_WITH_TOP) ->
 sig
  type t' =
  | Bot
  | Top_except of L.t PTree.t

  type t = t'

  val get : positive -> t -> L.t

  val set : positive -> L.t -> t -> t

  val beq : t -> t -> bool

  val bot : t'

  val top : t'

  module LM :
   sig
    type t = L.t PTree.t

    val get : positive -> t -> L.t

    val set : positive -> L.t -> t -> t

    val beq : t -> t -> bool

    val bot : t

    val opt_beq : L.t option -> L.t option -> bool

    type changed =
    | Same
    | Same1
    | Same2
    | Changed of L.t PTree.t

    val xcombine :
      (L.t option -> L.t option -> L.t option) -> L.t PTree.tree -> L.t
      PTree.tree -> changed

    val combine :
      (L.t option -> L.t option -> L.t option) -> L.t PTree.t -> L.t PTree.t
      -> L.t PTree.t

    val lub : t -> t -> t
   end

  val opt_lub : L.t -> L.t -> L.t option

  val lub : t -> t -> t
 end

module LFSet :
 functor (S:WS) ->
 sig
  type t = S.t

  val beq : t -> t -> bool

  val bot : t

  val lub : t -> t -> t
 end

module LBoolean :
 sig
  type t = bool

  val beq : t -> t -> bool

  val bot : bool

  val top : bool

  val lub : t -> t -> bool
 end
