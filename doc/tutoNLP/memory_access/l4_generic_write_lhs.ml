open Optitrust
open Prelude

(* Difficulty: Level 4
   Request: target the assignment that writes to s
   Expected target: [cWrite ~lhs:[cVar "s"] ()]
   Accepted variant: [cWriteVar "s"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cWrite ~lhs:[cVar "s"] ()];
)
