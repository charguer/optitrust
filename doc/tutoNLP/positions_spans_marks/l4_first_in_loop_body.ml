open Optitrust
open Prelude

(* Difficulty: Level 4
   Request: target the first position inside the bi loop body
   Expected target: [cForBody "bi"; tFirst]
   Accepted variant: [tFirst; cForBody "bi"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cForBody "bi"; tFirst];
)
