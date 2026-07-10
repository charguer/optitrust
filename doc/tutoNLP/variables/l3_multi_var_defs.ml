open Optitrust
open Prelude

(* Difficulty: Level 3
   Request: target the definitions of ixx, ixy, and iyy
   Expected target: [multi cVarDef ["ixx"; "ixy"; "iyy"]]
   Accepted variant: [cVarDefs ["ixx"; "ixy"; "iyy"]] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [multi cVarDef ["ixx"; "ixy"; "iyy"]];
)
