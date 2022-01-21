open AST
open BinNums
open Datatypes
open Integers
open Machregs
open Op

type label = positive

type instruction =
| Mgetstack of Ptrofs.int * typ * mreg
| Msetstack of mreg * Ptrofs.int * typ
| Mgetparam of Ptrofs.int * typ * mreg
| Mop of operation * mreg list * mreg
| Mload of memory_chunk * addressing * mreg list * mreg
| Mstore of memory_chunk * addressing * mreg list * mreg
| Mcall of signature * (mreg, ident) sum
| Mtailcall of signature * (mreg, ident) sum
| Mbuiltin of external_function * mreg builtin_arg list * mreg builtin_res
| Mlabel of label
| Mgoto of label
| Mcond of condition * mreg list * label
| Mjumptable of mreg * label list
| Mreturn

type code = instruction list

type coq_function = { fn_sig : signature; fn_code : code;
                      fn_stacksize : coq_Z; fn_link_ofs : Ptrofs.int;
                      fn_retaddr_ofs : Ptrofs.int }

type fundef = coq_function AST.fundef

type program = (fundef, unit) AST.program
