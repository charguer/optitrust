open Optitrust
open Prelude

(* Difficulty: Level 3
   Request: target both loops i and j
   Expected target: [multi cFor ["i"; "j"]]
   Accepted variant: [cFors ["i"; "j"]] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [multi cFor ["i"; "j"]];
)
