open Optitrust
open Prelude

(* Difficulty: Level 1
   Request: target the definition of tmp
   Expected target: [cVarDef "tmp"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cVarDef "tmp"];
)
