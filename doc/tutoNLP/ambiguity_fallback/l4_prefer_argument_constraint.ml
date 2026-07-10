open Optitrust
open Prelude

(* Difficulty: Level 4
   Request: target swap(a, b)
   Expected target: [cCall "swap" ~args:[[cVar "a"]; [cVar "b"]]]
   Rejected target: [sInstr "swap(a, b);"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cCall "swap" ~args:[[cVar "a"]; [cVar "b"]]];
)
