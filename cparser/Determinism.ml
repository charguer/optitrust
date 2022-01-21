open AST
open Events
open Integers

type world = __world Lazy.t
and __world =
| World of (char list -> eventval list -> (eventval * world) option)
   * (memory_chunk -> ident -> Ptrofs.int -> (eventval * world) option)
   * (memory_chunk -> ident -> Ptrofs.int -> eventval -> world option)

(** val nextworld_vload :
    world -> memory_chunk -> ident -> Ptrofs.int -> (eventval * world) option **)

let nextworld_vload w chunk id ofs =
  let World (_, vl, _) = Lazy.force w in vl chunk id ofs

(** val nextworld_vstore :
    world -> memory_chunk -> ident -> Ptrofs.int -> eventval -> world option **)

let nextworld_vstore w chunk id ofs v =
  let World (_, _, vs) = Lazy.force w in vs chunk id ofs v
