open AST
open Events
open Integers

type world = __world Lazy.t
and __world =
| World of (char list -> eventval list -> (eventval * world) option)
   * (memory_chunk -> ident -> Ptrofs.int -> (eventval * world) option)
   * (memory_chunk -> ident -> Ptrofs.int -> eventval -> world option)

val nextworld_vload :
  world -> memory_chunk -> ident -> Ptrofs.int -> (eventval * world) option

val nextworld_vstore :
  world -> memory_chunk -> ident -> Ptrofs.int -> eventval -> world option
