open AST
open Floats
open Integers

type eventval =
| EVint of Int.int
| EVlong of Int64.int
| EVfloat of float
| EVsingle of float32
| EVptr_global of ident * Ptrofs.int

type event =
| Event_syscall of char list * eventval list * eventval
| Event_vload of memory_chunk * ident * Ptrofs.int * eventval
| Event_vstore of memory_chunk * ident * Ptrofs.int * eventval
| Event_annot of char list * eventval list

type trace = event list

val coq_E0 : trace
