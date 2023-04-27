open BinNums
open BinPos
open Datatypes
open PeanoNat

type 'a coq_Comparable =
  'a -> 'a -> comparison
  (* singleton inductive, whose constructor was Build_Comparable *)

(** val compare : 'a1 coq_Comparable -> 'a1 -> 'a1 -> comparison **)

let compare comparable =
  comparable

(** val natComparable : nat coq_Comparable **)

let natComparable =
  Nat.compare

(** val coq_PairComparable :
    'a1 coq_Comparable -> 'a2 coq_Comparable -> ('a1 * 'a2) coq_Comparable **)

let coq_PairComparable cA cB x y =
  let (xa, xb) = x in
  let (ya, yb) = y in
  (match compare cA xa ya with
   | Eq -> compare cB xb yb
   | x0 -> x0)

(** val compare_eqb : 'a1 coq_Comparable -> 'a1 -> 'a1 -> bool **)

let compare_eqb c x y =
  match compare c x y with
  | Eq -> true
  | _ -> false

type 'a coq_Finite =
  'a list
  (* singleton inductive, whose constructor was Build_Finite *)

(** val all_list : 'a1 coq_Finite -> 'a1 list **)

let all_list finite =
  finite

type 'a coq_Alphabet = { coq_AlphabetComparable : 'a coq_Comparable;
                         coq_AlphabetFinite : 'a coq_Finite }

type 'a coq_Numbered = { inj : ('a -> positive); surj : (positive -> 'a);
                         inj_bound : positive }

(** val coq_NumberedAlphabet : 'a1 coq_Numbered -> 'a1 coq_Alphabet **)

let coq_NumberedAlphabet n =
  { coq_AlphabetComparable = (fun x y -> Pos.compare (n.inj x) (n.inj y));
    coq_AlphabetFinite =
    (fst
      (Pos.iter (fun pat ->
        let (l, p) = pat in (((n.surj p) :: l), (Pos.succ p))) ([], Coq_xH)
        n.inj_bound)) }

module type ComparableM =
 sig
  type t

  val tComparable : t coq_Comparable
 end

module OrderedTypeAlt_from_ComparableM =
 functor (C:ComparableM) ->
 struct
  type t = C.t

  (** val compare : t -> t -> comparison **)

  let compare =
    compare C.tComparable
 end

module OrderedType_from_ComparableM =
 functor (C:ComparableM) ->
 struct
  module Alt = OrderedTypeAlt_from_ComparableM(C)

  type t = Alt.t

  (** val compare : Alt.t -> Alt.t -> Alt.t OrderedType.coq_Compare **)

  let compare x y =
    match Alt.compare x y with
    | Eq -> OrderedType.EQ
    | Lt -> OrderedType.LT
    | Gt -> OrderedType.GT

  (** val eq_dec : Alt.t -> Alt.t -> bool **)

  let eq_dec x y =
    match Alt.compare x y with
    | Eq -> true
    | _ -> false
 end
