open Optitrust
open Prelude

(* Difficulty: Level 5
   Request: target the whole sequence inside the bi loop
   Expected target: [tSpanSeq [cForBody "bi"]]
   Accepted variant: [cForBody "bi"; tBetweenAll] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [tSpanSeq [cForBody "bi"]];
)
