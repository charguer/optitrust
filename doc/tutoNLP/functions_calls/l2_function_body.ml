open Optitrust
open Prelude

(* Difficulty: Level 2
   Request: target the body of setup
   Expected target: [cFunBody "setup"]
   Accepted variant: [cTopFunBody "setup"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFunBody "setup"];
)
