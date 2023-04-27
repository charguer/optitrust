open AST
open BinNums
open Coqlib
open Csyntax
open Ctypes
open Globalenvs
open List0
open Maps
open Memory
open Values

type genv = { genv_genv : (Csyntax.fundef, coq_type) Genv.t;
              genv_cenv : composite_env }

val globalenv : Csyntax.program -> genv

type env = (block * coq_type) PTree.t

val empty_env : env

val block_of_binding :
  genv -> (ident * (block * coq_type)) -> (block * coq_Z) * coq_Z

val blocks_of_env : genv -> env -> ((block * coq_Z) * coq_Z) list

val select_switch_default : labeled_statements -> labeled_statements

val select_switch_case :
  coq_Z -> labeled_statements -> labeled_statements option

val select_switch : coq_Z -> labeled_statements -> labeled_statements

val seq_of_labeled_statement : labeled_statements -> statement

type kind =
| LV
| RV

type cont =
| Kstop
| Kdo of cont
| Kseq of statement * cont
| Kifthenelse of statement * statement * cont
| Kwhile1 of expr * statement * cont
| Kwhile2 of expr * statement * cont
| Kdowhile1 of expr * statement * cont
| Kdowhile2 of expr * statement * cont
| Kfor2 of expr * statement * statement * cont
| Kfor3 of expr * statement * statement * cont
| Kfor4 of expr * statement * statement * cont
| Kswitch1 of labeled_statements * cont
| Kswitch2 of cont
| Kreturn of cont
| Kcall of coq_function * env * (expr -> expr) * coq_type * cont

val call_cont : cont -> cont

type state =
| State of coq_function * statement * cont * env * Mem.mem
| ExprState of coq_function * expr * cont * env * Mem.mem
| Callstate of Csyntax.fundef * coq_val list * cont * Mem.mem
| Returnstate of coq_val * cont * Mem.mem
| Stuckstate

val find_label : label -> statement -> cont -> (statement * cont) option

val find_label_ls :
  label -> labeled_statements -> cont -> (statement * cont) option
