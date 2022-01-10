(* Marks for use in transformations *)
type t = string(* fresh marks *)

let next_int : unit -> int =
  Tools.fresh_generator()

let next : unit -> t =
  fun () -> "__" ^ string_of_int (next_int()) (* a prefix for generated marks *)

