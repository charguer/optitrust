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

(** val globalenv : Csyntax.program -> genv **)

let globalenv p =
  { genv_genv = (Genv.globalenv (program_of_program p)); genv_cenv =
    p.prog_comp_env }

type env = (block * coq_type) PTree.t

(** val empty_env : env **)

let empty_env =
  PTree.empty

(** val block_of_binding :
    genv -> (ident * (block * coq_type)) -> (block * coq_Z) * coq_Z **)

let block_of_binding ge = function
| (_, p) -> let (b, ty) = p in ((b, Z0), (sizeof ge.genv_cenv ty))

(** val blocks_of_env : genv -> env -> ((block * coq_Z) * coq_Z) list **)

let blocks_of_env ge e =
  map (block_of_binding ge) (PTree.elements e)

(** val select_switch_default : labeled_statements -> labeled_statements **)

let rec select_switch_default sl = match sl with
| LSnil -> sl
| LScons (o, _, sl') ->
  (match o with
   | Some _ -> select_switch_default sl'
   | None -> sl)

(** val select_switch_case :
    coq_Z -> labeled_statements -> labeled_statements option **)

let rec select_switch_case n sl = match sl with
| LSnil -> None
| LScons (o, _, sl') ->
  (match o with
   | Some c -> if zeq c n then Some sl else select_switch_case n sl'
   | None -> select_switch_case n sl')

(** val select_switch : coq_Z -> labeled_statements -> labeled_statements **)

let select_switch n sl =
  match select_switch_case n sl with
  | Some sl' -> sl'
  | None -> select_switch_default sl

(** val seq_of_labeled_statement : labeled_statements -> statement **)

let rec seq_of_labeled_statement = function
| LSnil -> Sskip
| LScons (_, s, sl') -> Ssequence (s, (seq_of_labeled_statement sl'))

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

(** val call_cont : cont -> cont **)

let rec call_cont k = match k with
| Kdo k0 -> k0
| Kseq (_, k0) -> call_cont k0
| Kifthenelse (_, _, k0) -> call_cont k0
| Kwhile1 (_, _, k0) -> call_cont k0
| Kwhile2 (_, _, k0) -> call_cont k0
| Kdowhile1 (_, _, k0) -> call_cont k0
| Kdowhile2 (_, _, k0) -> call_cont k0
| Kfor2 (_, _, _, k0) -> call_cont k0
| Kfor3 (_, _, _, k0) -> call_cont k0
| Kfor4 (_, _, _, k0) -> call_cont k0
| Kswitch1 (_, k0) -> call_cont k0
| Kswitch2 k0 -> call_cont k0
| Kreturn k0 -> call_cont k0
| _ -> k

type state =
| State of coq_function * statement * cont * env * Mem.mem
| ExprState of coq_function * expr * cont * env * Mem.mem
| Callstate of Csyntax.fundef * coq_val list * cont * Mem.mem
| Returnstate of coq_val * cont * Mem.mem
| Stuckstate

(** val find_label :
    label -> statement -> cont -> (statement * cont) option **)

let rec find_label lbl s k =
  match s with
  | Ssequence (s1, s2) ->
    (match find_label lbl s1 (Kseq (s2, k)) with
     | Some sk -> Some sk
     | None -> find_label lbl s2 k)
  | Sifthenelse (_, s1, s2) ->
    (match find_label lbl s1 k with
     | Some sk -> Some sk
     | None -> find_label lbl s2 k)
  | Swhile (a, s1) -> find_label lbl s1 (Kwhile2 (a, s1, k))
  | Sdowhile (a, s1) -> find_label lbl s1 (Kdowhile1 (a, s1, k))
  | Sfor (a1, a2, a3, s1) ->
    (match find_label lbl a1 (Kseq ((Sfor (Sskip, a2, a3, s1)), k)) with
     | Some sk -> Some sk
     | None ->
       (match find_label lbl s1 (Kfor3 (a2, a3, s1, k)) with
        | Some sk -> Some sk
        | None -> find_label lbl a3 (Kfor4 (a2, a3, s1, k))))
  | Sswitch (_, sl) -> find_label_ls lbl sl (Kswitch2 k)
  | Slabel (lbl', s') ->
    if ident_eq lbl lbl' then Some (s', k) else find_label lbl s' k
  | _ -> None

(** val find_label_ls :
    label -> labeled_statements -> cont -> (statement * cont) option **)

and find_label_ls lbl sl k =
  match sl with
  | LSnil -> None
  | LScons (_, s, sl') ->
    (match find_label lbl s (Kseq ((seq_of_labeled_statement sl'), k)) with
     | Some sk -> Some sk
     | None -> find_label_ls lbl sl' k)
