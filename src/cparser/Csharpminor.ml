open AST
open BinNums
open Cminor
open Datatypes
open Floats
open Integers

type constant =
| Ointconst of Int.int
| Ofloatconst of float
| Osingleconst of float32
| Olongconst of Int64.int

type unary_operation = Cminor.unary_operation

type binary_operation = Cminor.binary_operation

type expr =
| Evar of ident
| Eaddrof of ident
| Econst of constant
| Eunop of unary_operation * expr
| Ebinop of binary_operation * expr * expr
| Eload of memory_chunk * expr

type label = ident

type stmt =
| Sskip
| Sset of ident * expr
| Sstore of memory_chunk * expr * expr
| Scall of ident option * signature * expr * expr list
| Sbuiltin of ident option * external_function * expr list
| Sseq of stmt * stmt
| Sifthenelse of expr * stmt * stmt
| Sloop of stmt
| Sblock of stmt
| Sexit of nat
| Sswitch of bool * expr * lbl_stmt
| Sreturn of expr option
| Slabel of label * stmt
| Sgoto of label
and lbl_stmt =
| LSnil
| LScons of coq_Z option * stmt * lbl_stmt

type coq_function = { fn_sig : signature; fn_params : ident list;
                      fn_vars : (ident * coq_Z) list; fn_temps : ident list;
                      fn_body : stmt }

type fundef = coq_function AST.fundef

type program = (fundef, unit) AST.program
