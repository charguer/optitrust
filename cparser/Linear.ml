open AST
open BinNums
open Datatypes
open Locations
open Machregs
open Op

type label = positive

type instruction =
| Lgetstack of slot * coq_Z * typ * mreg
| Lsetstack of mreg * slot * coq_Z * typ
| Lop of operation * mreg list * mreg
| Lload of memory_chunk * addressing * mreg list * mreg
| Lstore of memory_chunk * addressing * mreg list * mreg
| Lcall of signature * (mreg, ident) sum
| Ltailcall of signature * (mreg, ident) sum
| Lbuiltin of external_function * loc builtin_arg list * mreg builtin_res
| Llabel of label
| Lgoto of label
| Lcond of condition * mreg list * label
| Ljumptable of mreg * label list
| Lreturn

type code = instruction list

type coq_function = { fn_sig : signature; fn_stacksize : coq_Z; fn_code : code }

type fundef = coq_function AST.fundef

type program = (fundef, unit) AST.program
