open BinInt
open BinNums
open Coqlib
open Datatypes
open Integers
open Maps
open PeanoNat

type table = (coq_Z * nat) list

type comptree =
| CTaction of nat
| CTifeq of coq_Z * nat * comptree
| CTiflt of coq_Z * comptree * comptree
| CTjumptable of coq_Z * coq_Z * nat list * comptree

(** val split_lt : coq_Z -> table -> table * table **)

let rec split_lt pivot = function
| [] -> ([], [])
| p :: rem ->
  let (key, act) = p in
  let (l, r) = split_lt pivot rem in
  if zlt key pivot then (((key, act) :: l), r) else (l, ((key, act) :: r))

(** val split_eq : coq_Z -> table -> nat option * table **)

let rec split_eq pivot = function
| [] -> (None, [])
| p :: rem ->
  let (key, act) = p in
  let (same, others) = split_eq pivot rem in
  if zeq key pivot
  then ((Some act), others)
  else (same, ((key, act) :: others))

(** val split_between :
    coq_Z -> nat -> coq_Z -> coq_Z -> table -> nat ZMap.t * table **)

let rec split_between modulus0 dfl ofs sz = function
| [] -> ((ZMap.init dfl), [])
| p :: rem ->
  let (key, act) = p in
  let (inside, outside) = split_between modulus0 dfl ofs sz rem in
  if zlt (Z.modulo (Z.sub key ofs) modulus0) sz
  then ((ZMap.set key act inside), outside)
  else (inside, ((key, act) :: outside))

(** val refine_low_bound : coq_Z -> coq_Z -> coq_Z **)

let refine_low_bound v lo =
  if zeq v lo then Z.add lo (Zpos Coq_xH) else lo

(** val refine_high_bound : coq_Z -> coq_Z -> coq_Z **)

let refine_high_bound v hi =
  if zeq v hi then Z.sub hi (Zpos Coq_xH) else hi

(** val validate_jumptable : nat ZMap.t -> nat list -> coq_Z -> bool **)

let rec validate_jumptable cases tbl n =
  match tbl with
  | [] -> true
  | act :: rem ->
    (&&) (Nat.eqb act (ZMap.get n cases))
      (validate_jumptable cases rem (Z.succ n))

(** val validate :
    coq_Z -> nat -> table -> comptree -> coq_Z -> coq_Z -> bool **)

let rec validate modulus0 default cases t0 lo hi =
  match t0 with
  | CTaction act ->
    (match cases with
     | [] -> Nat.eqb act default
     | p :: _ ->
       let (key1, act1) = p in
       (&&) ((&&) ((fun x -> x) (zeq key1 lo)) ((fun x -> x) (zeq lo hi)))
         (Nat.eqb act act1))
  | CTifeq (pivot, act, t') ->
    (&&)
      ((&&) ((fun x -> x) (zle Z0 pivot)) ((fun x -> x) (zlt pivot modulus0)))
      (let (o, others) = split_eq pivot cases in
       (match o with
        | Some act' ->
          (&&) (Nat.eqb act act')
            (validate modulus0 default others t' (refine_low_bound pivot lo)
              (refine_high_bound pivot hi))
        | None -> false))
  | CTiflt (pivot, t1, t2) ->
    (&&)
      ((&&) ((fun x -> x) (zle Z0 pivot)) ((fun x -> x) (zlt pivot modulus0)))
      (let (lcases, rcases) = split_lt pivot cases in
       (&&)
         (validate modulus0 default lcases t1 lo (Z.sub pivot (Zpos Coq_xH)))
         (validate modulus0 default rcases t2 pivot hi))
  | CTjumptable (ofs, sz, tbl, t') ->
    let tbl_len = list_length_z tbl in
    (&&)
      ((&&)
        ((&&)
          ((&&)
            ((&&)
              ((&&)
                ((&&) ((fun x -> x) (zle Z0 ofs))
                  ((fun x -> x) (zlt ofs modulus0)))
                ((fun x -> x) (zle Z0 sz))) ((fun x -> x) (zlt sz modulus0)))
            ((fun x -> x) (zle (Z.add ofs sz) modulus0)))
          ((fun x -> x) (zle sz tbl_len)))
        ((fun x -> x) (zlt sz Int.modulus)))
      (let (inside, outside) = split_between modulus0 default ofs sz cases in
       (&&) (validate_jumptable inside tbl ofs)
         (validate modulus0 default outside t' lo hi))

(** val validate_switch : coq_Z -> nat -> table -> comptree -> bool **)

let validate_switch modulus0 default cases t0 =
  validate modulus0 default cases t0 Z0 (Z.sub modulus0 (Zpos Coq_xH))
