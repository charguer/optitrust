open Optitrust
open Prelude

(* Difficulty: Level 4
   Request: target the loop y whose body contains out[y]
   Expected target: [cFor "y" ~body:[cArrayWrite "out"]]
   Rejected target: [cFor "y" ~body:[sExpr "out[y]"]] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFor "y" ~body:[cArrayWrite "out"]];
)
