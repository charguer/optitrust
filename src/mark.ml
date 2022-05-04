(* [t]: marks used for transformations *)
type t = string(* fresh marks *)

(* [next_int]: generates an integer *)
let next_int : unit -> int =
  Tools.fresh_generator()

(* [next]: creates a mark based on [next_int] generator *)
let next : unit -> t =
  fun () -> "__" ^ string_of_int (next_int()) (* a prefix for generated marks *)
