open Optitrust
open Prelude

(* Difficulty: Level 2
   Request: target the position after the loop i
   Expected target: [cFor "i"; tAfter]
   Accepted variant: [tAfter; cFor "i"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFor "i"; tAfter];
)
