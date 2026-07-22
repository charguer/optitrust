open Optitrust
open Prelude

(* Difficulty: Level 2
   Request: target the first y loop in blur
   Expected target: [occFirst; cFunDef "blur"; cFor "y"]
   Accepted variant: [cFunDef "blur"; occFirst; cFor "y"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [occFirst; cFunDef "blur"; cFor "y"];
)
