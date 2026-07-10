open Optitrust
open Prelude

(* Difficulty: Level 1
   Request: target writes to A
   Expected target: [nbMulti; cArrayWrite "A"]
   Accepted variant: [cArrayWrite "A"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [nbMulti; cArrayWrite "A"];
)
