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

val eq_list_valnum : valnum list -> valnum list -> bool

val eq_rhs : rhs -> rhs -> bool

type numbering = { num_next : valnum; num_eqs : equation list;
                   num_reg : valnum PTree.t; num_val : reg list PMap.t }

val empty_numbering : numbering
