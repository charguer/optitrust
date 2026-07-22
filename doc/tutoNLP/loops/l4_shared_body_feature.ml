open Optitrust
open Prelude

(* Difficulty: Level 4
   Request: target every y loop that writes to ix or iy
   Expected target: [nbMulti; cFor "y" ~body:[any cArrayWrite ["ix"; "iy"]]]
   Accepted variant: [nbExact 2; cFor "y" ~body:[any cArrayWrite ["ix"; "iy"]]] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [nbMulti; cFor "y" ~body:[any cArrayWrite ["ix"; "iy"]]];
)
