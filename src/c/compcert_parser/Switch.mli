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

val split_lt : coq_Z -> table -> table * table

val split_eq : coq_Z -> table -> nat option * table

val split_between :
  coq_Z -> nat -> coq_Z -> coq_Z -> table -> nat ZMap.t * table

val refine_low_bound : coq_Z -> coq_Z -> coq_Z

val refine_high_bound : coq_Z -> coq_Z -> coq_Z

val validate_jumptable : nat ZMap.t -> nat list -> coq_Z -> bool

val validate : coq_Z -> nat -> table -> comptree -> coq_Z -> coq_Z -> bool

val validate_switch : coq_Z -> nat -> table -> comptree -> bool
