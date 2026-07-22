open Optitrust
open Prelude

(* Difficulty: Level 2
   Request: target the initializer of tile
   Expected target: [cVarInit "tile"]
   Accepted variant: [cVarDef "tile"; dVarBody] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cVarInit "tile"];
)
