open BinNums
open Coqlib
open Datatypes
open EquivDec
open List0

type __ = Obj.t

module type TREE =
 sig
  type elt

  val elt_eq : elt -> elt -> bool

  type 'x t

  val empty : 'a1 t

  val get : elt -> 'a1 t -> 'a1 option

  val set : elt -> 'a1 -> 'a1 t -> 'a1 t

  val remove : elt -> 'a1 t -> 'a1 t

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val map : (elt -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val combine :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t

  val elements : 'a1 t -> (elt * 'a1) list

  val fold : ('a2 -> elt -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val fold1 : ('a2 -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
 end

module PTree :
 sig
  type elt = positive

  val elt_eq : positive -> positive -> bool

  type 'a tree' =
  | Node001 of 'a tree'
  | Node010 of 'a
  | Node011 of 'a * 'a tree'
  | Node100 of 'a tree'
  | Node101 of 'a tree' * 'a tree'
  | Node110 of 'a tree' * 'a
  | Node111 of 'a tree' * 'a * 'a tree'

  type 'a tree =
  | Empty
  | Nodes of 'a tree'

  type 'a t = 'a tree

  val coq_Node : 'a1 tree -> 'a1 option -> 'a1 tree -> 'a1 tree

  val empty : 'a1 t

  val get' : positive -> 'a1 tree' -> 'a1 option

  val get : positive -> 'a1 tree -> 'a1 option

  val set0 : positive -> 'a1 -> 'a1 tree'

  val set' : positive -> 'a1 -> 'a1 tree' -> 'a1 tree'

  val set : positive -> 'a1 -> 'a1 tree -> 'a1 tree

  val rem' : positive -> 'a1 tree' -> 'a1 tree

  val remove' : positive -> 'a1 tree' -> 'a1 tree

  val remove : positive -> 'a1 tree -> 'a1 tree

  val tree_case :
    'a2 -> ('a1 tree -> 'a1 option -> 'a1 tree -> 'a2) -> 'a1 tree -> 'a2

  val tree_rec' :
    'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> 'a2) -> 'a1
    tree' -> 'a2

  val tree_rec :
    'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> 'a2) -> 'a1
    tree -> 'a2

  val tree_rec2' :
    'a3 -> ('a2 tree -> 'a3) -> ('a1 tree -> 'a3) -> ('a1 tree -> 'a1 option
    -> 'a1 tree -> 'a2 tree -> 'a2 option -> 'a2 tree -> 'a3 -> 'a3 -> 'a3)
    -> 'a1 tree' -> 'a2 tree' -> 'a3

  val tree_rec2 :
    'a3 -> ('a2 tree -> 'a3) -> ('a1 tree -> 'a3) -> ('a1 tree -> 'a1 option
    -> 'a1 tree -> 'a2 tree -> 'a2 option -> 'a2 tree -> 'a3 -> 'a3 -> 'a3)
    -> 'a1 tree -> 'a2 tree -> 'a3

  val tree_ind' :
    'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> __ -> 'a2) ->
    'a1 tree' -> 'a2

  val tree_ind :
    'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> __ -> 'a2) ->
    'a1 tree -> 'a2

  val beq' : ('a1 -> 'a1 -> bool) -> 'a1 tree' -> 'a1 tree' -> bool

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val prev_append : positive -> positive -> positive

  val prev : positive -> positive

  val map' : (positive -> 'a1 -> 'a2) -> 'a1 tree' -> positive -> 'a2 tree'

  val map : (positive -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree

  val map1' : ('a1 -> 'a2) -> 'a1 tree' -> 'a2 tree'

  val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t

  val map_filter1_nonopt : ('a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree

  val map_filter1 : ('a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree

  val filter1 : ('a1 -> bool) -> 'a1 t -> 'a1 t

  val combine :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree -> 'a3
    tree

  val xelements' :
    'a1 tree' -> positive -> (positive * 'a1) list -> (positive * 'a1) list

  val elements : 'a1 t -> (positive * 'a1) list

  val xelements : 'a1 t -> positive -> (positive * 'a1) list

  val xkeys : 'a1 t -> positive -> positive list

  val fold' :
    ('a2 -> positive -> 'a1 -> 'a2) -> positive -> 'a1 tree' -> 'a2 -> 'a2

  val fold : ('a2 -> positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2

  val fold1' : ('a2 -> 'a1 -> 'a2) -> 'a1 tree' -> 'a2 -> 'a2

  val fold1 : ('a2 -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
 end

module PMap :
 sig
  type 'a t = 'a * 'a PTree.t

  val init : 'a1 -> 'a1 * 'a1 PTree.t

  val get : positive -> 'a1 t -> 'a1

  val set : positive -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> positive

  val eq : t -> t -> bool
 end

module IMap :
 functor (X:INDEXED_TYPE) ->
 sig
  type elt = X.t

  val elt_eq : X.t -> X.t -> bool

  type 'x t = 'x PMap.t

  val init : 'a1 -> 'a1 * 'a1 PTree.t

  val get : X.t -> 'a1 t -> 'a1

  val set : X.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module ZIndexed :
 sig
  type t = coq_Z

  val index : coq_Z -> positive

  val eq : coq_Z -> coq_Z -> bool
 end

module ZMap :
 sig
  type elt = ZIndexed.t

  val elt_eq : ZIndexed.t -> ZIndexed.t -> bool

  type 'x t = 'x PMap.t

  val init : 'a1 -> 'a1 * 'a1 PTree.t

  val get : ZIndexed.t -> 'a1 t -> 'a1

  val set : ZIndexed.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree

  val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end

module ITree :
 functor (X:INDEXED_TYPE) ->
 sig
  type elt = X.t

  val elt_eq : X.t -> X.t -> bool

  type 'x t = 'x PTree.t

  val empty : 'a1 t

  val get : elt -> 'a1 t -> 'a1 option

  val set : elt -> 'a1 -> 'a1 t -> 'a1 t

  val remove : elt -> 'a1 t -> 'a1 t

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val combine :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t
 end

module ZTree :
 sig
  type elt = ZIndexed.t

  val elt_eq : ZIndexed.t -> ZIndexed.t -> bool

  type 'x t = 'x PTree.t

  val empty : 'a1 t

  val get : elt -> 'a1 t -> 'a1 option

  val set : elt -> 'a1 -> 'a1 t -> 'a1 t

  val remove : elt -> 'a1 t -> 'a1 t

  val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool

  val combine :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t
 end

module Tree_Properties :
 functor (T:TREE) ->
 sig
  val fold_ind_aux :
    ('a2 -> T.elt -> 'a1 -> 'a2) -> 'a2 -> 'a1 T.t -> ('a1 T.t -> __ -> 'a3)
    -> ('a1 T.t -> 'a2 -> T.elt -> 'a1 -> __ -> __ -> 'a3 -> 'a3) ->
    (T.elt * 'a1) list -> 'a1 T.t -> 'a3

  val fold_ind :
    ('a2 -> T.elt -> 'a1 -> 'a2) -> 'a2 -> 'a1 T.t -> ('a1 T.t -> __ -> 'a3)
    -> ('a1 T.t -> 'a2 -> T.elt -> 'a1 -> __ -> __ -> 'a3 -> 'a3) -> 'a3

  val cardinal : 'a1 T.t -> nat

  val for_all : 'a1 T.t -> (T.elt -> 'a1 -> bool) -> bool

  val exists_ : 'a1 T.t -> (T.elt -> 'a1 -> bool) -> bool

  val coq_Equal_dec : 'a1 coq_EqDec -> 'a1 T.t -> 'a1 T.t -> bool

  val coq_Equal_EqDec : 'a1 coq_EqDec -> 'a1 T.t coq_EqDec

  val of_list : (T.elt * 'a1) list -> 'a1 T.t
 end

module PTree_Properties :
 sig
  val fold_ind_aux :
    ('a2 -> PTree.elt -> 'a1 -> 'a2) -> 'a2 -> 'a1 PTree.t -> ('a1 PTree.t ->
    __ -> 'a3) -> ('a1 PTree.t -> 'a2 -> PTree.elt -> 'a1 -> __ -> __ -> 'a3
    -> 'a3) -> (PTree.elt * 'a1) list -> 'a1 PTree.t -> 'a3

  val fold_ind :
    ('a2 -> PTree.elt -> 'a1 -> 'a2) -> 'a2 -> 'a1 PTree.t -> ('a1 PTree.t ->
    __ -> 'a3) -> ('a1 PTree.t -> 'a2 -> PTree.elt -> 'a1 -> __ -> __ -> 'a3
    -> 'a3) -> 'a3

  val cardinal : 'a1 PTree.t -> nat

  val for_all : 'a1 PTree.t -> (PTree.elt -> 'a1 -> bool) -> bool

  val exists_ : 'a1 PTree.t -> (PTree.elt -> 'a1 -> bool) -> bool

  val coq_Equal_dec : 'a1 coq_EqDec -> 'a1 PTree.t -> 'a1 PTree.t -> bool

  val coq_Equal_EqDec : 'a1 coq_EqDec -> 'a1 PTree.t coq_EqDec

  val of_list : (PTree.elt * 'a1) list -> 'a1 PTree.t
 end
