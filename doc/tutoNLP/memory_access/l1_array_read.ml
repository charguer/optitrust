open Optitrust
open Prelude

(* Difficulty: Level 1
   Request: target reads from A
   Expected target: [nbMulti; cArrayRead "A"]
   Accepted variant: [cArrayRead "A"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [nbMulti; cArrayRead "A"];
)
