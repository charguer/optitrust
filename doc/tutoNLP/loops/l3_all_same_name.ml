open Optitrust
open Prelude

(* Difficulty: Level 3
   Request: target every loop named x in blur
   Expected target: [nbMulti; cFunDef "blur"; cFor "x"]
   Accepted variant: [nbMulti; cFunBody "blur"; cFor "x"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [nbMulti; cFunDef "blur"; cFor "x"];
)
