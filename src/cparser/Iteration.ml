open BinNums
open BinPos
open Coqlib
open Datatypes
open Specif

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module WfIter =
 struct
  (** val step_info :
      ('a1 -> ('a2, 'a1) sum) -> 'a1 -> ('a2, 'a1 sig2) sum **)

  let step_info step a =
    match step a with
    | Coq_inl b -> Coq_inl b
    | Coq_inr a0 -> Coq_inr a0

  (** val iterate_F :
      ('a1 -> ('a2, 'a1) sum) -> 'a1 -> ('a1 -> __ -> 'a2) -> 'a2 **)

  let iterate_F step a rec0 =
    match step_info step a with
    | Coq_inl s -> s
    | Coq_inr s -> rec0 s __

  (** val iterate : ('a1 -> ('a2, 'a1) sum) -> 'a1 -> 'a2 **)

  let rec iterate step a =
    iterate_F step a (fun y _ -> iterate step y)
 end

module PrimIter =
 struct
  (** val num_iterations : positive **)

  let num_iterations =
    Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO
      (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI (Coq_xO
      (Coq_xI (Coq_xO (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xO (Coq_xO (Coq_xI
      (Coq_xO (Coq_xI (Coq_xO (Coq_xI (Coq_xI (Coq_xO (Coq_xO (Coq_xO (Coq_xI
      (Coq_xO (Coq_xI (Coq_xI Coq_xH))))))))))))))))))))))))))))))))))))))

  (** val iter_step :
      ('a1 -> ('a2, 'a1) sum) -> positive -> (positive -> __ -> 'a1 -> 'a2
      option) -> 'a1 -> 'a2 option **)

  let iter_step step x next s =
    if peq x Coq_xH
    then None
    else (match step s with
          | Coq_inl res -> Some res
          | Coq_inr s' -> next (Pos.pred x) __ s')

  (** val iter : ('a1 -> ('a2, 'a1) sum) -> positive -> 'a1 -> 'a2 option **)

  let rec iter step x =
    iter_step step x (fun y _ -> iter step y)

  (** val iterate : ('a1 -> ('a2, 'a1) sum) -> 'a1 -> 'a2 option **)

  let iterate step =
    iter step num_iterations
 end
