open Optitrust
open Prelude

(* Difficulty: Level 3
   Request: target the two loops named i
   Expected target: [nbExact 2; cFor "i"]
   Accepted variant: [nbMulti; cFor "i"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [nbExact 2; cFor "i"];
)
