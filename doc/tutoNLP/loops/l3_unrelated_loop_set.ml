open Optitrust
open Prelude

(* Difficulty: Level 3
   Request: target the loops p, q, and r
   Expected target: [multi cFor ["p"; "q"; "r"]]
   Accepted variant: [cFors ["p"; "q"; "r"]] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [multi cFor ["p"; "q"; "r"]];
)
