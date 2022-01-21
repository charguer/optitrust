open BinNums
open NeedDomain
open Op

val op1 : nval -> nval list

val op2 : nval -> nval list

val needs_of_condition : condition -> nval list

val needs_of_addressing_32 : addressing -> nval -> nval list

val needs_of_addressing_64 : addressing -> nval -> nval list

val needs_of_operation : operation -> nval -> nval list

val operation_is_redundant : operation -> nval -> bool
