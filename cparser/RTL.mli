open AST
open BinNums
open BinPos
open Datatypes
open List0
open Maps
open Op
open Registers

type node = positive

type instruction =
| Inop of node
| Iop of operation * reg list * reg * node
| Iload of memory_chunk * addressing * reg list * reg * node
| Istore of memory_chunk * addressing * reg list * reg * node
| Icall of signature * (reg, ident) sum * reg list * reg * node
| Itailcall of signature * (reg, ident) sum * reg list
| Ibuiltin of external_function * reg builtin_arg list * reg builtin_res
   * node
| Icond of condition * reg list * node * node
| Ijumptable of reg * node list
| Ireturn of reg option

type code = instruction PTree.t

type coq_function = { fn_sig : signature; fn_params : reg list;
                      fn_stacksize : coq_Z; fn_code : code;
                      fn_entrypoint : node }

type fundef = coq_function AST.fundef

type program = (fundef, unit) AST.program

val transf_function :
  (node -> instruction -> instruction) -> coq_function -> coq_function

val successors_instr : instruction -> node list

val successors_map : coq_function -> node list PTree.t

val instr_uses : instruction -> reg list

val instr_defs : instruction -> reg option

val max_pc_function : coq_function -> positive

val max_reg_instr : positive -> node -> instruction -> positive

val max_reg_function : coq_function -> positive
