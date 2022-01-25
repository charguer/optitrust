open AST
open BinNums
open Datatypes
open Locations
open Machregs
open Maps
open Op

type node = positive

type instruction =
| Lop of operation * mreg list * mreg
| Lload of memory_chunk * addressing * mreg list * mreg
| Lgetstack of slot * coq_Z * typ * mreg
| Lsetstack of mreg * slot * coq_Z * typ
| Lstore of memory_chunk * addressing * mreg list * mreg
| Lcall of signature * (mreg, ident) sum
| Ltailcall of signature * (mreg, ident) sum
| Lbuiltin of external_function * loc builtin_arg list * mreg builtin_res
| Lbranch of node
| Lcond of condition * mreg list * node * node
| Ljumptable of mreg * node list
| Lreturn

type bblock = instruction list

type code = bblock PTree.t

type coq_function = { fn_sig : signature; fn_stacksize : coq_Z;
                      fn_code : code; fn_entrypoint : node }

type fundef = coq_function AST.fundef

type program = (fundef, unit) AST.program

(** val destroyed_by_getstack : slot -> mreg list **)

let destroyed_by_getstack = function
| Incoming -> temp_for_parent_frame :: []
| _ -> []

(** val successors_block : bblock -> node list **)

let rec successors_block = function
| [] -> []
| instr :: b' ->
  (match instr with
   | Ltailcall (_, _) -> []
   | Lbranch s -> s :: []
   | Lcond (_, _, s1, s2) -> s1 :: (s2 :: [])
   | Ljumptable (_, tbl) -> tbl
   | Lreturn -> []
   | _ -> successors_block b')
