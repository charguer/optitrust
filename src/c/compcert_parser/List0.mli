open Datatypes

val hd : 'a1 -> 'a1 list -> 'a1

val tl : 'a1 list -> 'a1 list

val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val nth_error : 'a1 list -> nat -> 'a1 option

val remove : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list

val rev : 'a1 list -> 'a1 list

val rev_append : 'a1 list -> 'a1 list -> 'a1 list

val rev' : 'a1 list -> 'a1 list

val list_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1

val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1

val existsb : ('a1 -> bool) -> 'a1 list -> bool

val forallb : ('a1 -> bool) -> 'a1 list -> bool

val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list

val repeat : 'a1 -> nat -> 'a1 list
