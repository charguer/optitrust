open BinInt
open BinNums
open BinPos
open Datatypes
open List0
open ZArith_dec

val peq : positive -> positive -> bool

val plt : positive -> positive -> bool

val zeq : coq_Z -> coq_Z -> bool

val zlt : coq_Z -> coq_Z -> bool

val zle : coq_Z -> coq_Z -> bool

val align : coq_Z -> coq_Z -> coq_Z

val floor : coq_Z -> coq_Z -> coq_Z

val option_eq : ('a1 -> 'a1 -> bool) -> 'a1 option -> 'a1 option -> bool

val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option

val sum_left_map : ('a1 -> 'a2) -> ('a1, 'a3) sum -> ('a2, 'a3) sum

val list_length_z_aux : 'a1 list -> coq_Z -> coq_Z

val list_length_z : 'a1 list -> coq_Z

val list_nth_z : 'a1 list -> coq_Z -> 'a1 option

val list_fold_left : ('a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 list -> 'a2

val list_fold_right : ('a1 -> 'a2 -> 'a2) -> 'a1 list -> 'a2 -> 'a2

val list_disjoint_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

val list_norepet_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> bool


