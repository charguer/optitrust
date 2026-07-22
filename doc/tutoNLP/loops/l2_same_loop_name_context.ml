open Optitrust
open Prelude

(* Difficulty: Level 2
   Request: target the loop i inside main_loop
   Expected target: [cFunBody "main_loop"; cFor "i"]
   Accepted variant: [cTopFunDef "main_loop"; cFor "i"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFunBody "main_loop"; cFor "i"];
)
