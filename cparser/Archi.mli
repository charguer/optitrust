open BinNums
open Datatypes

val ptr64 : bool

val big_endian : bool

val align_int64 : coq_Z

val align_float64 : coq_Z

val splitlong : bool

val default_nan_64 : bool * positive

val default_nan_32 : bool * positive

val choose_nan_64 : (bool * positive) list -> bool * positive

val choose_nan_32 : (bool * positive) list -> bool * positive

val float_of_single_preserves_sNaN : bool

val win64 : bool

(*val abi : abi_kind*)