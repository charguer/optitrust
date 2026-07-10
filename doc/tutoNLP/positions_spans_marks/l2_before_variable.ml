open Optitrust
open Prelude

(* Difficulty: Level 2
   Request: target the position before variable c is declared
   Expected target: [tBefore; cVarDef "c"]
   Rejected target: [cVarDef "c"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [tBefore; cVarDef "c"];
)
