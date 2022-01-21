open Archi
open BinNums
open Compopts
open Integers
open Op
open ValueDomain

val eval_static_condition : condition -> aval list -> abool

val eval_static_addressing_32 : addressing -> aval list -> aval

val eval_static_addressing_64 : addressing -> aval list -> aval

val eval_static_addressing : addressing -> aval list -> aval

val eval_static_operation : operation -> aval list -> aval
