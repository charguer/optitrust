open Optitrust
open Prelude

(* Difficulty: Level 5
   Request: target the if condition k + 1 < 10
   Expected target: [cIf ~cond:[sExpr "k + 1 < 10"] ()]
   Accepted variant: [cIf ~cond:[sExpr "k+1 < 10"] ()] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cIf ~cond:[sExpr "k + 1 < 10"] ()];
)
