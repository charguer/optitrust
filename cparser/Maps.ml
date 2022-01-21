open BinNums
open Coqlib
open Datatypes
open EquivDec
open List0

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

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

module PTree =
 struct
  type elt = positive

  (** val elt_eq : positive -> positive -> bool **)

  let elt_eq =
    peq

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

  (** val coq_Node : 'a1 tree -> 'a1 option -> 'a1 tree -> 'a1 tree **)

  let coq_Node l o r =
    match l with
    | Empty ->
      (match o with
       | Some x ->
         (match r with
          | Empty -> Nodes (Node010 x)
          | Nodes r' -> Nodes (Node011 (x, r')))
       | None ->
         (match r with
          | Empty -> Empty
          | Nodes r' -> Nodes (Node001 r')))
    | Nodes l' ->
      (match o with
       | Some x ->
         (match r with
          | Empty -> Nodes (Node110 (l', x))
          | Nodes r' -> Nodes (Node111 (l', x, r')))
       | None ->
         (match r with
          | Empty -> Nodes (Node100 l')
          | Nodes r' -> Nodes (Node101 (l', r'))))

  (** val empty : 'a1 t **)

  let empty =
    Empty

  (** val get' : positive -> 'a1 tree' -> 'a1 option **)

  let rec get' p m =
    match p with
    | Coq_xI q ->
      (match m with
       | Node001 m' -> get' q m'
       | Node011 (_, m') -> get' q m'
       | Node101 (_, m') -> get' q m'
       | Node111 (_, _, m') -> get' q m'
       | _ -> None)
    | Coq_xO q ->
      (match m with
       | Node100 m' -> get' q m'
       | Node101 (m', _) -> get' q m'
       | Node110 (m', _) -> get' q m'
       | Node111 (m', _, _) -> get' q m'
       | _ -> None)
    | Coq_xH ->
      (match m with
       | Node010 x -> Some x
       | Node011 (x, _) -> Some x
       | Node110 (_, x) -> Some x
       | Node111 (_, x, _) -> Some x
       | _ -> None)

  (** val get : positive -> 'a1 tree -> 'a1 option **)

  let get p = function
  | Empty -> None
  | Nodes m' -> get' p m'

  (** val set0 : positive -> 'a1 -> 'a1 tree' **)

  let rec set0 p x =
    match p with
    | Coq_xI q -> Node001 (set0 q x)
    | Coq_xO q -> Node100 (set0 q x)
    | Coq_xH -> Node010 x

  (** val set' : positive -> 'a1 -> 'a1 tree' -> 'a1 tree' **)

  let rec set' p x m =
    match p with
    | Coq_xI q ->
      (match m with
       | Node001 r -> Node001 (set' q x r)
       | Node010 y -> Node011 (y, (set0 q x))
       | Node011 (y, r) -> Node011 (y, (set' q x r))
       | Node100 l -> Node101 (l, (set0 q x))
       | Node101 (l, r) -> Node101 (l, (set' q x r))
       | Node110 (l, y) -> Node111 (l, y, (set0 q x))
       | Node111 (l, y, r) -> Node111 (l, y, (set' q x r)))
    | Coq_xO q ->
      (match m with
       | Node001 r -> Node101 ((set0 q x), r)
       | Node010 y -> Node110 ((set0 q x), y)
       | Node011 (y, r) -> Node111 ((set0 q x), y, r)
       | Node100 l -> Node100 (set' q x l)
       | Node101 (l, r) -> Node101 ((set' q x l), r)
       | Node110 (l, y) -> Node110 ((set' q x l), y)
       | Node111 (l, y, r) -> Node111 ((set' q x l), y, r))
    | Coq_xH ->
      (match m with
       | Node001 r -> Node011 (x, r)
       | Node010 _ -> Node010 x
       | Node011 (_, r) -> Node011 (x, r)
       | Node100 l -> Node110 (l, x)
       | Node101 (l, r) -> Node111 (l, x, r)
       | Node110 (l, _) -> Node110 (l, x)
       | Node111 (l, _, r) -> Node111 (l, x, r))

  (** val set : positive -> 'a1 -> 'a1 tree -> 'a1 tree **)

  let set p x = function
  | Empty -> Nodes (set0 p x)
  | Nodes m' -> Nodes (set' p x m')

  (** val rem' : positive -> 'a1 tree' -> 'a1 tree **)

  let rec rem' p m =
    match p with
    | Coq_xI q ->
      (match m with
       | Node001 r -> coq_Node Empty None (rem' q r)
       | Node011 (y, r) -> coq_Node Empty (Some y) (rem' q r)
       | Node101 (l, r) -> coq_Node (Nodes l) None (rem' q r)
       | Node111 (l, y, r) -> coq_Node (Nodes l) (Some y) (rem' q r)
       | _ -> Nodes m)
    | Coq_xO q ->
      (match m with
       | Node100 l -> coq_Node (rem' q l) None Empty
       | Node101 (l, r) -> coq_Node (rem' q l) None (Nodes r)
       | Node110 (l, y) -> coq_Node (rem' q l) (Some y) Empty
       | Node111 (l, y, r) -> coq_Node (rem' q l) (Some y) (Nodes r)
       | _ -> Nodes m)
    | Coq_xH ->
      (match m with
       | Node010 _ -> Empty
       | Node011 (_, r) -> Nodes (Node001 r)
       | Node110 (l, _) -> Nodes (Node100 l)
       | Node111 (l, _, r) -> Nodes (Node101 (l, r))
       | _ -> Nodes m)

  (** val remove' : positive -> 'a1 tree' -> 'a1 tree **)

  let rec remove' p m =
    match p with
    | Coq_xI q ->
      (match m with
       | Node001 r ->
         (match remove' q r with
          | Empty -> Empty
          | Nodes r' -> Nodes (Node001 r'))
       | Node011 (y, r) ->
         (match remove' q r with
          | Empty -> Nodes (Node010 y)
          | Nodes r' -> Nodes (Node011 (y, r')))
       | Node101 (l, r) ->
         (match remove' q r with
          | Empty -> Nodes (Node100 l)
          | Nodes r' -> Nodes (Node101 (l, r')))
       | Node111 (l, y, r) ->
         (match remove' q r with
          | Empty -> Nodes (Node110 (l, y))
          | Nodes r' -> Nodes (Node111 (l, y, r')))
       | _ -> Nodes m)
    | Coq_xO q ->
      (match m with
       | Node100 l ->
         (match remove' q l with
          | Empty -> Empty
          | Nodes l' -> Nodes (Node100 l'))
       | Node101 (l, r) ->
         (match remove' q l with
          | Empty -> Nodes (Node001 r)
          | Nodes l' -> Nodes (Node101 (l', r)))
       | Node110 (l, y) ->
         (match remove' q l with
          | Empty -> Nodes (Node010 y)
          | Nodes l' -> Nodes (Node110 (l', y)))
       | Node111 (l, y, r) ->
         (match remove' q l with
          | Empty -> Nodes (Node011 (y, r))
          | Nodes l' -> Nodes (Node111 (l', y, r)))
       | _ -> Nodes m)
    | Coq_xH ->
      (match m with
       | Node010 _ -> Empty
       | Node011 (_, r) -> Nodes (Node001 r)
       | Node110 (l, _) -> Nodes (Node100 l)
       | Node111 (l, _, r) -> Nodes (Node101 (l, r))
       | _ -> Nodes m)

  (** val remove : positive -> 'a1 tree -> 'a1 tree **)

  let remove p = function
  | Empty -> Empty
  | Nodes m' -> remove' p m'

  (** val tree_case :
      'a2 -> ('a1 tree -> 'a1 option -> 'a1 tree -> 'a2) -> 'a1 tree -> 'a2 **)

  let tree_case empty0 node = function
  | Empty -> empty0
  | Nodes t0 ->
    (match t0 with
     | Node001 r -> node Empty None (Nodes r)
     | Node010 x -> node Empty (Some x) Empty
     | Node011 (x, r) -> node Empty (Some x) (Nodes r)
     | Node100 l -> node (Nodes l) None Empty
     | Node101 (l, r) -> node (Nodes l) None (Nodes r)
     | Node110 (l, x) -> node (Nodes l) (Some x) Empty
     | Node111 (l, x, r) -> node (Nodes l) (Some x) (Nodes r))

  (** val tree_rec' :
      'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> 'a2) -> 'a1
      tree' -> 'a2 **)

  let rec tree_rec' empty0 node = function
  | Node001 r -> node Empty empty0 None (Nodes r) (tree_rec' empty0 node r)
  | Node010 x -> node Empty empty0 (Some x) Empty empty0
  | Node011 (x, r) ->
    node Empty empty0 (Some x) (Nodes r) (tree_rec' empty0 node r)
  | Node100 l -> node (Nodes l) (tree_rec' empty0 node l) None Empty empty0
  | Node101 (l, r) ->
    node (Nodes l) (tree_rec' empty0 node l) None (Nodes r)
      (tree_rec' empty0 node r)
  | Node110 (l, x) ->
    node (Nodes l) (tree_rec' empty0 node l) (Some x) Empty empty0
  | Node111 (l, x, r) ->
    node (Nodes l) (tree_rec' empty0 node l) (Some x) (Nodes r)
      (tree_rec' empty0 node r)

  (** val tree_rec :
      'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> 'a2) -> 'a1
      tree -> 'a2 **)

  let tree_rec empty0 node = function
  | Empty -> empty0
  | Nodes m' -> tree_rec' empty0 node m'

  (** val tree_rec2' :
      'a3 -> ('a2 tree -> 'a3) -> ('a1 tree -> 'a3) -> ('a1 tree -> 'a1
      option -> 'a1 tree -> 'a2 tree -> 'a2 option -> 'a2 tree -> 'a3 -> 'a3
      -> 'a3) -> 'a1 tree' -> 'a2 tree' -> 'a3 **)

  let rec tree_rec2' base base1 base2 nodes m1 m2 =
    match m1 with
    | Node001 r1 ->
      (match m2 with
       | Node001 r2 ->
         nodes Empty None (Nodes r1) Empty None (Nodes r2) base
           (tree_rec2' base base1 base2 nodes r1 r2)
       | Node010 x2 ->
         nodes Empty None (Nodes r1) Empty (Some x2) Empty base
           (base2 (Nodes r1))
       | Node011 (x2, r2) ->
         nodes Empty None (Nodes r1) Empty (Some x2) (Nodes r2) base
           (tree_rec2' base base1 base2 nodes r1 r2)
       | Node100 l2 ->
         nodes Empty None (Nodes r1) (Nodes l2) None Empty (base1 (Nodes l2))
           (base2 (Nodes r1))
       | Node101 (l2, r2) ->
         nodes Empty None (Nodes r1) (Nodes l2) None (Nodes r2)
           (base1 (Nodes l2)) (tree_rec2' base base1 base2 nodes r1 r2)
       | Node110 (l2, x2) ->
         nodes Empty None (Nodes r1) (Nodes l2) (Some x2) Empty
           (base1 (Nodes l2)) (base2 (Nodes r1))
       | Node111 (l2, x2, r2) ->
         nodes Empty None (Nodes r1) (Nodes l2) (Some x2) (Nodes r2)
           (base1 (Nodes l2)) (tree_rec2' base base1 base2 nodes r1 r2))
    | Node010 x1 ->
      (match m2 with
       | Node001 r2 ->
         nodes Empty (Some x1) Empty Empty None (Nodes r2) base
           (base1 (Nodes r2))
       | Node010 x2 ->
         nodes Empty (Some x1) Empty Empty (Some x2) Empty base base
       | Node011 (x2, r2) ->
         nodes Empty (Some x1) Empty Empty (Some x2) (Nodes r2) base
           (base1 (Nodes r2))
       | Node100 l2 ->
         nodes Empty (Some x1) Empty (Nodes l2) None Empty (base1 (Nodes l2))
           base
       | Node101 (l2, r2) ->
         nodes Empty (Some x1) Empty (Nodes l2) None (Nodes r2)
           (base1 (Nodes l2)) (base1 (Nodes r2))
       | Node110 (l2, x2) ->
         nodes Empty (Some x1) Empty (Nodes l2) (Some x2) Empty
           (base1 (Nodes l2)) base
       | Node111 (l2, x2, r2) ->
         nodes Empty (Some x1) Empty (Nodes l2) (Some x2) (Nodes r2)
           (base1 (Nodes l2)) (base1 (Nodes r2)))
    | Node011 (x1, r1) ->
      (match m2 with
       | Node001 r2 ->
         nodes Empty (Some x1) (Nodes r1) Empty None (Nodes r2) base
           (tree_rec2' base base1 base2 nodes r1 r2)
       | Node010 x2 ->
         nodes Empty (Some x1) (Nodes r1) Empty (Some x2) Empty base
           (base2 (Nodes r1))
       | Node011 (x2, r2) ->
         nodes Empty (Some x1) (Nodes r1) Empty (Some x2) (Nodes r2) base
           (tree_rec2' base base1 base2 nodes r1 r2)
       | Node100 l2 ->
         nodes Empty (Some x1) (Nodes r1) (Nodes l2) None Empty
           (base1 (Nodes l2)) (base2 (Nodes r1))
       | Node101 (l2, r2) ->
         nodes Empty (Some x1) (Nodes r1) (Nodes l2) None (Nodes r2)
           (base1 (Nodes l2)) (tree_rec2' base base1 base2 nodes r1 r2)
       | Node110 (l2, x2) ->
         nodes Empty (Some x1) (Nodes r1) (Nodes l2) (Some x2) Empty
           (base1 (Nodes l2)) (base2 (Nodes r1))
       | Node111 (l2, x2, r2) ->
         nodes Empty (Some x1) (Nodes r1) (Nodes l2) (Some x2) (Nodes r2)
           (base1 (Nodes l2)) (tree_rec2' base base1 base2 nodes r1 r2))
    | Node100 l1 ->
      (match m2 with
       | Node001 r2 ->
         nodes (Nodes l1) None Empty Empty None (Nodes r2) (base2 (Nodes l1))
           (base1 (Nodes r2))
       | Node010 x2 ->
         nodes (Nodes l1) None Empty Empty (Some x2) Empty (base2 (Nodes l1))
           base
       | Node011 (x2, r2) ->
         nodes (Nodes l1) None Empty Empty (Some x2) (Nodes r2)
           (base2 (Nodes l1)) (base1 (Nodes r2))
       | Node100 l2 ->
         nodes (Nodes l1) None Empty (Nodes l2) None Empty
           (tree_rec2' base base1 base2 nodes l1 l2) base
       | Node101 (l2, r2) ->
         nodes (Nodes l1) None Empty (Nodes l2) None (Nodes r2)
           (tree_rec2' base base1 base2 nodes l1 l2) (base1 (Nodes r2))
       | Node110 (l2, x2) ->
         nodes (Nodes l1) None Empty (Nodes l2) (Some x2) Empty
           (tree_rec2' base base1 base2 nodes l1 l2) base
       | Node111 (l2, x2, r2) ->
         nodes (Nodes l1) None Empty (Nodes l2) (Some x2) (Nodes r2)
           (tree_rec2' base base1 base2 nodes l1 l2) (base1 (Nodes r2)))
    | Node101 (l1, r1) ->
      (match m2 with
       | Node001 r2 ->
         nodes (Nodes l1) None (Nodes r1) Empty None (Nodes r2)
           (base2 (Nodes l1)) (tree_rec2' base base1 base2 nodes r1 r2)
       | Node010 x2 ->
         nodes (Nodes l1) None (Nodes r1) Empty (Some x2) Empty
           (base2 (Nodes l1)) (base2 (Nodes r1))
       | Node011 (x2, r2) ->
         nodes (Nodes l1) None (Nodes r1) Empty (Some x2) (Nodes r2)
           (base2 (Nodes l1)) (tree_rec2' base base1 base2 nodes r1 r2)
       | Node100 l2 ->
         nodes (Nodes l1) None (Nodes r1) (Nodes l2) None Empty
           (tree_rec2' base base1 base2 nodes l1 l2) (base2 (Nodes r1))
       | Node101 (l2, r2) ->
         nodes (Nodes l1) None (Nodes r1) (Nodes l2) None (Nodes r2)
           (tree_rec2' base base1 base2 nodes l1 l2)
           (tree_rec2' base base1 base2 nodes r1 r2)
       | Node110 (l2, x2) ->
         nodes (Nodes l1) None (Nodes r1) (Nodes l2) (Some x2) Empty
           (tree_rec2' base base1 base2 nodes l1 l2) (base2 (Nodes r1))
       | Node111 (l2, x2, r2) ->
         nodes (Nodes l1) None (Nodes r1) (Nodes l2) (Some x2) (Nodes r2)
           (tree_rec2' base base1 base2 nodes l1 l2)
           (tree_rec2' base base1 base2 nodes r1 r2))
    | Node110 (l1, x1) ->
      (match m2 with
       | Node001 r2 ->
         nodes (Nodes l1) (Some x1) Empty Empty None (Nodes r2)
           (base2 (Nodes l1)) (base1 (Nodes r2))
       | Node010 x2 ->
         nodes (Nodes l1) (Some x1) Empty Empty (Some x2) Empty
           (base2 (Nodes l1)) base
       | Node011 (x2, r2) ->
         nodes (Nodes l1) (Some x1) Empty Empty (Some x2) (Nodes r2)
           (base2 (Nodes l1)) (base1 (Nodes r2))
       | Node100 l2 ->
         nodes (Nodes l1) (Some x1) Empty (Nodes l2) None Empty
           (tree_rec2' base base1 base2 nodes l1 l2) base
       | Node101 (l2, r2) ->
         nodes (Nodes l1) (Some x1) Empty (Nodes l2) None (Nodes r2)
           (tree_rec2' base base1 base2 nodes l1 l2) (base1 (Nodes r2))
       | Node110 (l2, x2) ->
         nodes (Nodes l1) (Some x1) Empty (Nodes l2) (Some x2) Empty
           (tree_rec2' base base1 base2 nodes l1 l2) base
       | Node111 (l2, x2, r2) ->
         nodes (Nodes l1) (Some x1) Empty (Nodes l2) (Some x2) (Nodes r2)
           (tree_rec2' base base1 base2 nodes l1 l2) (base1 (Nodes r2)))
    | Node111 (l1, x1, r1) ->
      (match m2 with
       | Node001 r2 ->
         nodes (Nodes l1) (Some x1) (Nodes r1) Empty None (Nodes r2)
           (base2 (Nodes l1)) (tree_rec2' base base1 base2 nodes r1 r2)
       | Node010 x2 ->
         nodes (Nodes l1) (Some x1) (Nodes r1) Empty (Some x2) Empty
           (base2 (Nodes l1)) (base2 (Nodes r1))
       | Node011 (x2, r2) ->
         nodes (Nodes l1) (Some x1) (Nodes r1) Empty (Some x2) (Nodes r2)
           (base2 (Nodes l1)) (tree_rec2' base base1 base2 nodes r1 r2)
       | Node100 l2 ->
         nodes (Nodes l1) (Some x1) (Nodes r1) (Nodes l2) None Empty
           (tree_rec2' base base1 base2 nodes l1 l2) (base2 (Nodes r1))
       | Node101 (l2, r2) ->
         nodes (Nodes l1) (Some x1) (Nodes r1) (Nodes l2) None (Nodes r2)
           (tree_rec2' base base1 base2 nodes l1 l2)
           (tree_rec2' base base1 base2 nodes r1 r2)
       | Node110 (l2, x2) ->
         nodes (Nodes l1) (Some x1) (Nodes r1) (Nodes l2) (Some x2) Empty
           (tree_rec2' base base1 base2 nodes l1 l2) (base2 (Nodes r1))
       | Node111 (l2, x2, r2) ->
         nodes (Nodes l1) (Some x1) (Nodes r1) (Nodes l2) (Some x2) (Nodes
           r2) (tree_rec2' base base1 base2 nodes l1 l2)
           (tree_rec2' base base1 base2 nodes r1 r2))

  (** val tree_rec2 :
      'a3 -> ('a2 tree -> 'a3) -> ('a1 tree -> 'a3) -> ('a1 tree -> 'a1
      option -> 'a1 tree -> 'a2 tree -> 'a2 option -> 'a2 tree -> 'a3 -> 'a3
      -> 'a3) -> 'a1 tree -> 'a2 tree -> 'a3 **)

  let tree_rec2 base base1 base2 nodes a b =
    match a with
    | Empty -> (match b with
                | Empty -> base
                | Nodes _ -> base1 b)
    | Nodes a' ->
      (match b with
       | Empty -> base2 a
       | Nodes b' -> tree_rec2' base base1 base2 nodes a' b')

  (** val tree_ind' :
      'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> __ -> 'a2)
      -> 'a1 tree' -> 'a2 **)

  let rec tree_ind' empty0 node = function
  | Node001 r -> node Empty empty0 None (Nodes r) (tree_ind' empty0 node r) __
  | Node010 x -> node Empty empty0 (Some x) Empty empty0 __
  | Node011 (x, r) ->
    node Empty empty0 (Some x) (Nodes r) (tree_ind' empty0 node r) __
  | Node100 l -> node (Nodes l) (tree_ind' empty0 node l) None Empty empty0 __
  | Node101 (l, r) ->
    node (Nodes l) (tree_ind' empty0 node l) None (Nodes r)
      (tree_ind' empty0 node r) __
  | Node110 (l, x) ->
    node (Nodes l) (tree_ind' empty0 node l) (Some x) Empty empty0 __
  | Node111 (l, x, r) ->
    node (Nodes l) (tree_ind' empty0 node l) (Some x) (Nodes r)
      (tree_ind' empty0 node r) __

  (** val tree_ind :
      'a2 -> ('a1 tree -> 'a2 -> 'a1 option -> 'a1 tree -> 'a2 -> __ -> 'a2)
      -> 'a1 tree -> 'a2 **)

  let tree_ind empty0 node = function
  | Empty -> empty0
  | Nodes m' -> tree_ind' empty0 node m'

  (** val beq' : ('a1 -> 'a1 -> bool) -> 'a1 tree' -> 'a1 tree' -> bool **)

  let rec beq' beqA m1 m2 =
    match m1 with
    | Node001 r1 -> (match m2 with
                     | Node001 r2 -> beq' beqA r1 r2
                     | _ -> false)
    | Node010 x1 -> (match m2 with
                     | Node010 x2 -> beqA x1 x2
                     | _ -> false)
    | Node011 (x1, r1) ->
      (match m2 with
       | Node011 (x2, r2) -> (&&) (beqA x1 x2) (beq' beqA r1 r2)
       | _ -> false)
    | Node100 l1 -> (match m2 with
                     | Node100 l2 -> beq' beqA l1 l2
                     | _ -> false)
    | Node101 (l1, r1) ->
      (match m2 with
       | Node101 (l2, r2) -> (&&) (beq' beqA l1 l2) (beq' beqA r1 r2)
       | _ -> false)
    | Node110 (l1, x1) ->
      (match m2 with
       | Node110 (l2, x2) -> (&&) (beqA x1 x2) (beq' beqA l1 l2)
       | _ -> false)
    | Node111 (l1, x1, r1) ->
      (match m2 with
       | Node111 (l2, x2, r2) ->
         (&&) ((&&) (beqA x1 x2) (beq' beqA l1 l2)) (beq' beqA r1 r2)
       | _ -> false)

  (** val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)

  let beq beqA m1 m2 =
    match m1 with
    | Empty -> (match m2 with
                | Empty -> true
                | Nodes _ -> false)
    | Nodes m1' ->
      (match m2 with
       | Empty -> false
       | Nodes m2' -> beq' beqA m1' m2')

  (** val prev_append : positive -> positive -> positive **)

  let rec prev_append i j =
    match i with
    | Coq_xI i' -> prev_append i' (Coq_xI j)
    | Coq_xO i' -> prev_append i' (Coq_xO j)
    | Coq_xH -> j

  (** val prev : positive -> positive **)

  let prev i =
    prev_append i Coq_xH

  (** val map' :
      (positive -> 'a1 -> 'a2) -> 'a1 tree' -> positive -> 'a2 tree' **)

  let rec map' f m i =
    match m with
    | Node001 r -> Node001 (map' f r (Coq_xI i))
    | Node010 x -> Node010 (f (prev i) x)
    | Node011 (x, r) -> Node011 ((f (prev i) x), (map' f r (Coq_xI i)))
    | Node100 l -> Node100 (map' f l (Coq_xO i))
    | Node101 (l, r) -> Node101 ((map' f l (Coq_xO i)), (map' f r (Coq_xI i)))
    | Node110 (l, x) -> Node110 ((map' f l (Coq_xO i)), (f (prev i) x))
    | Node111 (l, x, r) ->
      Node111 ((map' f l (Coq_xO i)), (f (prev i) x), (map' f r (Coq_xI i)))

  (** val map : (positive -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree **)

  let map f = function
  | Empty -> Empty
  | Nodes m0 -> Nodes (map' f m0 Coq_xH)

  (** val map1' : ('a1 -> 'a2) -> 'a1 tree' -> 'a2 tree' **)

  let rec map1' f = function
  | Node001 r -> Node001 (map1' f r)
  | Node010 x -> Node010 (f x)
  | Node011 (x, r) -> Node011 ((f x), (map1' f r))
  | Node100 l -> Node100 (map1' f l)
  | Node101 (l, r) -> Node101 ((map1' f l), (map1' f r))
  | Node110 (l, x) -> Node110 ((map1' f l), (f x))
  | Node111 (l, x, r) -> Node111 ((map1' f l), (f x), (map1' f r))

  (** val map1 : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map1 f = function
  | Empty -> Empty
  | Nodes m0 -> Nodes (map1' f m0)

  (** val map_filter1_nonopt : ('a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree **)

  let map_filter1_nonopt f = function
  | Empty -> Empty
  | Nodes m' ->
    tree_rec' Empty (fun _ lrec o _ rrec ->
      coq_Node lrec (match o with
                     | Some a -> f a
                     | None -> None) rrec) m'

  (** val map_filter1 : ('a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree **)

  let map_filter1 f = function
  | Empty -> Empty
  | Nodes m' ->
    let rec tree_rec'0 = function
    | Node001 r ->
      (match tree_rec'0 r with
       | Empty -> Empty
       | Nodes r' -> Nodes (Node001 r'))
    | Node010 x ->
      (match f x with
       | Some x0 -> Nodes (Node010 x0)
       | None -> Empty)
    | Node011 (x, r) ->
      (match f x with
       | Some x0 ->
         (match tree_rec'0 r with
          | Empty -> Nodes (Node010 x0)
          | Nodes r' -> Nodes (Node011 (x0, r')))
       | None ->
         (match tree_rec'0 r with
          | Empty -> Empty
          | Nodes r' -> Nodes (Node001 r')))
    | Node100 l ->
      (match tree_rec'0 l with
       | Empty -> Empty
       | Nodes l' -> Nodes (Node100 l'))
    | Node101 (l, r) ->
      (match tree_rec'0 l with
       | Empty ->
         (match tree_rec'0 r with
          | Empty -> Empty
          | Nodes r' -> Nodes (Node001 r'))
       | Nodes l' ->
         (match tree_rec'0 r with
          | Empty -> Nodes (Node100 l')
          | Nodes r' -> Nodes (Node101 (l', r'))))
    | Node110 (l, x) ->
      (match tree_rec'0 l with
       | Empty ->
         (match f x with
          | Some x0 -> Nodes (Node010 x0)
          | None -> Empty)
       | Nodes l' ->
         (match f x with
          | Some x0 -> Nodes (Node110 (l', x0))
          | None -> Nodes (Node100 l')))
    | Node111 (l, x, r) ->
      (match tree_rec'0 l with
       | Empty ->
         (match f x with
          | Some x0 ->
            (match tree_rec'0 r with
             | Empty -> Nodes (Node010 x0)
             | Nodes r' -> Nodes (Node011 (x0, r')))
          | None ->
            (match tree_rec'0 r with
             | Empty -> Empty
             | Nodes r' -> Nodes (Node001 r')))
       | Nodes l' ->
         (match f x with
          | Some x0 ->
            (match tree_rec'0 r with
             | Empty -> Nodes (Node110 (l', x0))
             | Nodes r' -> Nodes (Node111 (l', x0, r')))
          | None ->
            (match tree_rec'0 r with
             | Empty -> Nodes (Node100 l')
             | Nodes r' -> Nodes (Node101 (l', r')))))
    in tree_rec'0 m'

  (** val filter1 : ('a1 -> bool) -> 'a1 t -> 'a1 t **)

  let filter1 pred m =
    map_filter1 (fun a -> if pred a then Some a else None) m

  (** val combine :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree -> 'a3
      tree **)

  let combine f =
    let combine_l = map_filter1 (fun a -> f (Some a) None) in
    let combine_r = map_filter1 (fun b -> f None (Some b)) in
    (fun m1 m2 ->
    match m1 with
    | Empty -> (match m2 with
                | Empty -> Empty
                | Nodes _ -> combine_r m2)
    | Nodes a' ->
      (match m2 with
       | Empty -> combine_l m1
       | Nodes b' ->
         let rec tree_rec2'0 m3 m4 =
           match m3 with
           | Node001 r1 ->
             (match m4 with
              | Node001 r2 -> coq_Node Empty None (tree_rec2'0 r1 r2)
              | Node010 x2 ->
                coq_Node Empty (f None (Some x2)) (combine_l (Nodes r1))
              | Node011 (x2, r2) ->
                coq_Node Empty (f None (Some x2)) (tree_rec2'0 r1 r2)
              | Node100 l2 ->
                coq_Node (combine_r (Nodes l2)) None (combine_l (Nodes r1))
              | Node101 (l2, r2) ->
                coq_Node (combine_r (Nodes l2)) None (tree_rec2'0 r1 r2)
              | Node110 (l2, x2) ->
                coq_Node (combine_r (Nodes l2)) (f None (Some x2))
                  (combine_l (Nodes r1))
              | Node111 (l2, x2, r2) ->
                coq_Node (combine_r (Nodes l2)) (f None (Some x2))
                  (tree_rec2'0 r1 r2))
           | Node010 x1 ->
             (match m4 with
              | Node001 r2 ->
                coq_Node Empty (f (Some x1) None) (combine_r (Nodes r2))
              | Node010 x2 -> coq_Node Empty (f (Some x1) (Some x2)) Empty
              | Node011 (x2, r2) ->
                coq_Node Empty (f (Some x1) (Some x2)) (combine_r (Nodes r2))
              | Node100 l2 ->
                coq_Node (combine_r (Nodes l2)) (f (Some x1) None) Empty
              | Node101 (l2, r2) ->
                coq_Node (combine_r (Nodes l2)) (f (Some x1) None)
                  (combine_r (Nodes r2))
              | Node110 (l2, x2) ->
                coq_Node (combine_r (Nodes l2)) (f (Some x1) (Some x2)) Empty
              | Node111 (l2, x2, r2) ->
                coq_Node (combine_r (Nodes l2)) (f (Some x1) (Some x2))
                  (combine_r (Nodes r2)))
           | Node011 (x1, r1) ->
             (match m4 with
              | Node001 r2 ->
                coq_Node Empty (f (Some x1) None) (tree_rec2'0 r1 r2)
              | Node010 x2 ->
                coq_Node Empty (f (Some x1) (Some x2)) (combine_l (Nodes r1))
              | Node011 (x2, r2) ->
                coq_Node Empty (f (Some x1) (Some x2)) (tree_rec2'0 r1 r2)
              | Node100 l2 ->
                coq_Node (combine_r (Nodes l2)) (f (Some x1) None)
                  (combine_l (Nodes r1))
              | Node101 (l2, r2) ->
                coq_Node (combine_r (Nodes l2)) (f (Some x1) None)
                  (tree_rec2'0 r1 r2)
              | Node110 (l2, x2) ->
                coq_Node (combine_r (Nodes l2)) (f (Some x1) (Some x2))
                  (combine_l (Nodes r1))
              | Node111 (l2, x2, r2) ->
                coq_Node (combine_r (Nodes l2)) (f (Some x1) (Some x2))
                  (tree_rec2'0 r1 r2))
           | Node100 l1 ->
             (match m4 with
              | Node001 r2 ->
                coq_Node (combine_l (Nodes l1)) None (combine_r (Nodes r2))
              | Node010 x2 ->
                coq_Node (combine_l (Nodes l1)) (f None (Some x2)) Empty
              | Node011 (x2, r2) ->
                coq_Node (combine_l (Nodes l1)) (f None (Some x2))
                  (combine_r (Nodes r2))
              | Node100 l2 -> coq_Node (tree_rec2'0 l1 l2) None Empty
              | Node101 (l2, r2) ->
                coq_Node (tree_rec2'0 l1 l2) None (combine_r (Nodes r2))
              | Node110 (l2, x2) ->
                coq_Node (tree_rec2'0 l1 l2) (f None (Some x2)) Empty
              | Node111 (l2, x2, r2) ->
                coq_Node (tree_rec2'0 l1 l2) (f None (Some x2))
                  (combine_r (Nodes r2)))
           | Node101 (l1, r1) ->
             (match m4 with
              | Node001 r2 ->
                coq_Node (combine_l (Nodes l1)) None (tree_rec2'0 r1 r2)
              | Node010 x2 ->
                coq_Node (combine_l (Nodes l1)) (f None (Some x2))
                  (combine_l (Nodes r1))
              | Node011 (x2, r2) ->
                coq_Node (combine_l (Nodes l1)) (f None (Some x2))
                  (tree_rec2'0 r1 r2)
              | Node100 l2 ->
                coq_Node (tree_rec2'0 l1 l2) None (combine_l (Nodes r1))
              | Node101 (l2, r2) ->
                coq_Node (tree_rec2'0 l1 l2) None (tree_rec2'0 r1 r2)
              | Node110 (l2, x2) ->
                coq_Node (tree_rec2'0 l1 l2) (f None (Some x2))
                  (combine_l (Nodes r1))
              | Node111 (l2, x2, r2) ->
                coq_Node (tree_rec2'0 l1 l2) (f None (Some x2))
                  (tree_rec2'0 r1 r2))
           | Node110 (l1, x1) ->
             (match m4 with
              | Node001 r2 ->
                coq_Node (combine_l (Nodes l1)) (f (Some x1) None)
                  (combine_r (Nodes r2))
              | Node010 x2 ->
                coq_Node (combine_l (Nodes l1)) (f (Some x1) (Some x2)) Empty
              | Node011 (x2, r2) ->
                coq_Node (combine_l (Nodes l1)) (f (Some x1) (Some x2))
                  (combine_r (Nodes r2))
              | Node100 l2 ->
                coq_Node (tree_rec2'0 l1 l2) (f (Some x1) None) Empty
              | Node101 (l2, r2) ->
                coq_Node (tree_rec2'0 l1 l2) (f (Some x1) None)
                  (combine_r (Nodes r2))
              | Node110 (l2, x2) ->
                coq_Node (tree_rec2'0 l1 l2) (f (Some x1) (Some x2)) Empty
              | Node111 (l2, x2, r2) ->
                coq_Node (tree_rec2'0 l1 l2) (f (Some x1) (Some x2))
                  (combine_r (Nodes r2)))
           | Node111 (l1, x1, r1) ->
             (match m4 with
              | Node001 r2 ->
                coq_Node (combine_l (Nodes l1)) (f (Some x1) None)
                  (tree_rec2'0 r1 r2)
              | Node010 x2 ->
                coq_Node (combine_l (Nodes l1)) (f (Some x1) (Some x2))
                  (combine_l (Nodes r1))
              | Node011 (x2, r2) ->
                coq_Node (combine_l (Nodes l1)) (f (Some x1) (Some x2))
                  (tree_rec2'0 r1 r2)
              | Node100 l2 ->
                coq_Node (tree_rec2'0 l1 l2) (f (Some x1) None)
                  (combine_l (Nodes r1))
              | Node101 (l2, r2) ->
                coq_Node (tree_rec2'0 l1 l2) (f (Some x1) None)
                  (tree_rec2'0 r1 r2)
              | Node110 (l2, x2) ->
                coq_Node (tree_rec2'0 l1 l2) (f (Some x1) (Some x2))
                  (combine_l (Nodes r1))
              | Node111 (l2, x2, r2) ->
                coq_Node (tree_rec2'0 l1 l2) (f (Some x1) (Some x2))
                  (tree_rec2'0 r1 r2))
         in tree_rec2'0 a' b'))

  (** val xelements' :
      'a1 tree' -> positive -> (positive * 'a1) list -> (positive * 'a1) list **)

  let rec xelements' m i k =
    match m with
    | Node001 r -> xelements' r (Coq_xI i) k
    | Node010 x -> ((prev i), x) :: k
    | Node011 (x, r) -> ((prev i), x) :: (xelements' r (Coq_xI i) k)
    | Node100 l -> xelements' l (Coq_xO i) k
    | Node101 (l, r) -> xelements' l (Coq_xO i) (xelements' r (Coq_xI i) k)
    | Node110 (l, x) -> xelements' l (Coq_xO i) (((prev i), x) :: k)
    | Node111 (l, x, r) ->
      xelements' l (Coq_xO i) (((prev i), x) :: (xelements' r (Coq_xI i) k))

  (** val elements : 'a1 t -> (positive * 'a1) list **)

  let elements = function
  | Empty -> []
  | Nodes m' -> xelements' m' Coq_xH []

  (** val xelements : 'a1 t -> positive -> (positive * 'a1) list **)

  let xelements m i =
    match m with
    | Empty -> []
    | Nodes m' -> xelements' m' i []

  (** val xkeys : 'a1 t -> positive -> positive list **)

  let xkeys m i =
    List0.map fst (xelements m i)

  (** val fold' :
      ('a2 -> positive -> 'a1 -> 'a2) -> positive -> 'a1 tree' -> 'a2 -> 'a2 **)

  let rec fold' f i m v =
    match m with
    | Node001 r -> fold' f (Coq_xI i) r v
    | Node010 x -> f v (prev i) x
    | Node011 (x, r) -> fold' f (Coq_xI i) r (f v (prev i) x)
    | Node100 l -> fold' f (Coq_xO i) l v
    | Node101 (l, r) -> fold' f (Coq_xI i) r (fold' f (Coq_xO i) l v)
    | Node110 (l, x) -> f (fold' f (Coq_xO i) l v) (prev i) x
    | Node111 (l, x, r) ->
      fold' f (Coq_xI i) r (f (fold' f (Coq_xO i) l v) (prev i) x)

  (** val fold : ('a2 -> positive -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let fold f m v =
    match m with
    | Empty -> v
    | Nodes m' -> fold' f Coq_xH m' v

  (** val fold1' : ('a2 -> 'a1 -> 'a2) -> 'a1 tree' -> 'a2 -> 'a2 **)

  let rec fold1' f m v =
    match m with
    | Node001 r -> fold1' f r v
    | Node010 x -> f v x
    | Node011 (x, r) -> fold1' f r (f v x)
    | Node100 l -> fold1' f l v
    | Node101 (l, r) -> fold1' f r (fold1' f l v)
    | Node110 (l, x) -> f (fold1' f l v) x
    | Node111 (l, x, r) -> fold1' f r (f (fold1' f l v) x)

  (** val fold1 : ('a2 -> 'a1 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let fold1 f m v =
    match m with
    | Empty -> v
    | Nodes m' -> fold1' f m' v
 end

module PMap =
 struct
  type 'a t = 'a * 'a PTree.t

  (** val init : 'a1 -> 'a1 * 'a1 PTree.t **)

  let init x =
    (x, PTree.empty)

  (** val get : positive -> 'a1 t -> 'a1 **)

  let get i m =
    match PTree.get i (snd m) with
    | Some x -> x
    | None -> fst m

  (** val set : positive -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree **)

  let set i x m =
    ((fst m), (PTree.set i x (snd m)))

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map f m =
    ((f (fst m)), (PTree.map1 f (snd m)))
 end

module type INDEXED_TYPE =
 sig
  type t

  val index : t -> positive

  val eq : t -> t -> bool
 end

module IMap =
 functor (X:INDEXED_TYPE) ->
 struct
  type elt = X.t

  (** val elt_eq : X.t -> X.t -> bool **)

  let elt_eq =
    X.eq

  type 'x t = 'x PMap.t

  (** val init : 'a1 -> 'a1 * 'a1 PTree.t **)

  let init =
    PMap.init

  (** val get : X.t -> 'a1 t -> 'a1 **)

  let get i m =
    PMap.get (X.index i) m

  (** val set : X.t -> 'a1 -> 'a1 t -> 'a1 * 'a1 PTree.tree **)

  let set i v m =
    PMap.set (X.index i) v m

  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let map =
    PMap.map
 end

module ZIndexed =
 struct
  type t = coq_Z

  (** val index : coq_Z -> positive **)

  let index = function
  | Z0 -> Coq_xH
  | Zpos p -> Coq_xO p
  | Zneg p -> Coq_xI p

  (** val eq : coq_Z -> coq_Z -> bool **)

  let eq =
    zeq
 end

module ZMap = IMap(ZIndexed)

module ITree =
 functor (X:INDEXED_TYPE) ->
 struct
  type elt = X.t

  (** val elt_eq : X.t -> X.t -> bool **)

  let elt_eq =
    X.eq

  type 'x t = 'x PTree.t

  (** val empty : 'a1 t **)

  let empty =
    PTree.empty

  (** val get : elt -> 'a1 t -> 'a1 option **)

  let get k m =
    PTree.get (X.index k) m

  (** val set : elt -> 'a1 -> 'a1 t -> 'a1 t **)

  let set k v m =
    PTree.set (X.index k) v m

  (** val remove : elt -> 'a1 t -> 'a1 t **)

  let remove k m =
    PTree.remove (X.index k) m

  (** val beq : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)

  let beq =
    PTree.beq

  (** val combine :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)

  let combine =
    PTree.combine
 end

module ZTree = ITree(ZIndexed)

module Tree_Properties =
 functor (T:TREE) ->
 struct
  (** val fold_ind_aux :
      ('a2 -> T.elt -> 'a1 -> 'a2) -> 'a2 -> 'a1 T.t -> ('a1 T.t -> __ ->
      'a3) -> ('a1 T.t -> 'a2 -> T.elt -> 'a1 -> __ -> __ -> 'a3 -> 'a3) ->
      (T.elt * 'a1) list -> 'a1 T.t -> 'a3 **)

  let fold_ind_aux f init0 _ h_base h_rec l m =
    let f' = fun p a -> f a (fst p) (snd p) in
    let h_base' = fun m0 -> h_base m0 __ in
    let h_rec' = fun k v _ a hR m0 ->
      h_rec m0 a k v __ __ (hR (T.remove k m0) __)
    in
    let rec f0 = function
    | [] -> (fun _ _ m0 _ -> h_base' m0)
    | y :: l1 ->
      let iHl = f0 l1 in
      (fun _ _ m0 _ ->
      h_rec' (fst y) (snd y) l1 (fold_right f' init0 l1) (iHl __ __) m0)
    in f0 l __ __ m __

  (** val fold_ind :
      ('a2 -> T.elt -> 'a1 -> 'a2) -> 'a2 -> 'a1 T.t -> ('a1 T.t -> __ ->
      'a3) -> ('a1 T.t -> 'a2 -> T.elt -> 'a1 -> __ -> __ -> 'a3 -> 'a3) ->
      'a3 **)

  let fold_ind f init0 m_final h_base h_rec =
    let l' = rev (T.elements m_final) in
    fold_ind_aux f init0 m_final h_base h_rec l' m_final

  (** val cardinal : 'a1 T.t -> nat **)

  let cardinal x =
    length (T.elements x)

  (** val for_all : 'a1 T.t -> (T.elt -> 'a1 -> bool) -> bool **)

  let for_all m f =
    T.fold (fun b x a -> (&&) b (f x a)) m true

  (** val exists_ : 'a1 T.t -> (T.elt -> 'a1 -> bool) -> bool **)

  let exists_ m f =
    T.fold (fun b x a -> (||) b (f x a)) m false

  (** val coq_Equal_dec : 'a1 coq_EqDec -> 'a1 T.t -> 'a1 T.t -> bool **)

  let coq_Equal_dec eqAdec m1 m2 =
    let filtered_var =
      T.beq (fun a1 a2 -> (fun x -> x) (equiv_dec eqAdec a1 a2)) m1 m2
    in
    if filtered_var then true else false

  (** val coq_Equal_EqDec : 'a1 coq_EqDec -> 'a1 T.t coq_EqDec **)

  let coq_Equal_EqDec =
    coq_Equal_dec

  (** val of_list : (T.elt * 'a1) list -> 'a1 T.t **)

  let of_list l =
    let f = fun m k_v -> T.set (fst k_v) (snd k_v) m in fold_left f l T.empty
 end

module PTree_Properties = Tree_Properties(PTree)
