open Optitrust
open Prelude

(* Difficulty: Level 4
   Request: target the write to field x
   Expected target: [cFieldWrite ~field:"x" ()] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFieldWrite ~field:"x" ()];
)
