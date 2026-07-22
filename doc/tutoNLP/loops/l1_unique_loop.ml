open Optitrust
open Prelude

(* Difficulty: Level 1
   Request: target the loop i
   Expected target: [cFor "i"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFor "i"];
)
