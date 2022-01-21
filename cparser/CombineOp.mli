open Archi
open BinNums
open CSEdomain
open Integers
open Op

type valnum = positive

val combine_compimm_ne_0 :
  (valnum -> rhs option) -> valnum -> (condition * valnum list) option

val combine_compimm_eq_0 :
  (valnum -> rhs option) -> valnum -> (condition * valnum list) option

val combine_compimm_eq_1 :
  (valnum -> rhs option) -> valnum -> (condition * valnum list) option

val combine_compimm_ne_1 :
  (valnum -> rhs option) -> valnum -> (condition * valnum list) option

val combine_cond :
  (valnum -> rhs option) -> condition -> valnum list -> (condition * valnum
  list) option

val combine_addr_32 :
  (valnum -> rhs option) -> addressing -> valnum list -> (addressing * valnum
  list) option

val combine_addr_64 :
  (valnum -> rhs option) -> addressing -> valnum list -> (addressing * valnum
  list) option

val combine_addr :
  (valnum -> rhs option) -> addressing -> valnum list -> (addressing * valnum
  list) option

val combine_op :
  (valnum -> rhs option) -> operation -> valnum list -> (operation * valnum
  list) option
