open Optitrust
open Prelude

(* Difficulty: Level 4
   Request: target the y loop that writes to out
   Expected target: [cFor "y" ~body:[cArrayWrite "out"]] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFor "y" ~body:[cArrayWrite "out"]];
)
