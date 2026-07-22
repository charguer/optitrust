open Optitrust
open Prelude

(* Difficulty: Level 3
   Request: target every call to update
   Expected target: [nbMulti; cCall "update"]
   Accepted variant: [nbExact 3; cCall "update"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [nbMulti; cCall "update"];
)
