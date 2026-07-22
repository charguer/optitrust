open Optitrust
open Prelude

(* Difficulty: Level 1
   Request: target the function mm
   Expected target: [cFunDef "mm"]
   Accepted variant: [cTopFunDef "mm"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFunDef "mm"];
)
