open AST
open BinNums
open Datatypes
open Floats
open Integers

type constant =
| Ointconst of Int.int
| Ofloatconst of float
| Osingleconst of float32
| Olongconst of Int64.int
| Oaddrsymbol of ident * Ptrofs.int
| Oaddrstack of Ptrofs.int

type unary_operation =
| Ocast8unsigned
| Ocast8signed
| Ocast16unsigned
| Ocast16signed
| Onegint
| Onotint
| Onegf
| Oabsf
| Onegfs
| Oabsfs
| Osingleoffloat
| Ofloatofsingle
| Ointoffloat
| Ointuoffloat
| Ofloatofint
| Ofloatofintu
| Ointofsingle
| Ointuofsingle
| Osingleofint
| Osingleofintu
| Onegl
| Onotl
| Ointoflong
| Olongofint
| Olongofintu
| Olongoffloat
| Olonguoffloat
| Ofloatoflong
| Ofloatoflongu
| Olongofsingle
| Olonguofsingle
| Osingleoflong
| Osingleoflongu

type binary_operation =
| Oadd
| Osub
| Omul
| Odiv
| Odivu
| Omod
| Omodu
| Oand
| Oor
| Oxor
| Oshl
| Oshr
| Oshru
| Oaddf
| Osubf
| Omulf
| Odivf
| Oaddfs
| Osubfs
| Omulfs
| Odivfs
| Oaddl
| Osubl
| Omull
| Odivl
| Odivlu
| Omodl
| Omodlu
| Oandl
| Oorl
| Oxorl
| Oshll
| Oshrl
| Oshrlu
| Ocmp of comparison
| Ocmpu of comparison
| Ocmpf of comparison
| Ocmpfs of comparison
| Ocmpl of comparison
| Ocmplu of comparison

type expr =
| Evar of ident
| Econst of constant
| Eunop of unary_operation * expr
| Ebinop of binary_operation * expr * expr
| Eload of memory_chunk * expr

type label = ident

type stmt =
| Sskip
| Sassign of ident * expr
| Sstore of memory_chunk * expr * expr
| Scall of ident option * signature * expr * expr list
| Stailcall of signature * expr * expr list
| Sbuiltin of ident option * external_function * expr list
| Sseq of stmt * stmt
| Sifthenelse of expr * stmt * stmt
| Sloop of stmt
| Sblock of stmt
| Sexit of nat
| Sswitch of bool * expr * (coq_Z * nat) list * nat
| Sreturn of expr option
| Slabel of label * stmt
| Sgoto of label

type coq_function = { fn_sig : signature; fn_params : ident list;
                      fn_vars : ident list; fn_stackspace : coq_Z;
                      fn_body : stmt }

type fundef = coq_function AST.fundef

type program = (fundef, unit) AST.program
