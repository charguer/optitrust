open AST
open BinNums
open Coqlib
open List0
open Maps
open Op
open Registers

type valnum = positive

type rhs =
| Op of operation * valnum list
| Load of memory_chunk * addressing * valnum list

type equation =
| Eq of valnum * bool * rhs

(** val eq_list_valnum : valnum list -> valnum list -> bool **)

let eq_list_valnum =
  list_eq_dec peq

(** val eq_rhs : rhs -> rhs -> bool **)

let eq_rhs x y =
  match x with
  | Op (x0, x1) ->
    (match y with
     | Op (o0, l0) ->
       if eq_operation x0 o0 then eq_list_valnum x1 l0 else false
     | Load (_, _, _) -> false)
  | Load (x0, x1, x2) ->
    (match y with
     | Op (_, _) -> false
     | Load (m0, a0, l0) ->
       if chunk_eq x0 m0
       then if eq_addressing x1 a0 then eq_list_valnum x2 l0 else false
       else false)

type numbering = { num_next : valnum; num_eqs : equation list;
                   num_reg : valnum PTree.t; num_val : reg list PMap.t }

(** val empty_numbering : numbering **)

let empty_numbering =
  { num_next = Coq_xH; num_eqs = []; num_reg = PTree.empty; num_val =
    (PMap.init []) }
