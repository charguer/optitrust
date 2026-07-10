open Optitrust
open Prelude

(* Difficulty: Level 4
   Request: target the read of s in the update
   Expected target: [cReadVar "s"]
   Accepted variant: [cWrite ~lhs:[cVar "s"] (); dRHS; cReadVar "s"]
   Rejected target: [cVar "s"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cReadVar "s"];
)
