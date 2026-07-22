open Optitrust
open Prelude

(* Difficulty: Level 4
   Request: target the call to swap with arguments a and b
   Expected target: [cCall "swap" ~args:[[cVar "a"]; [cVar "b"]]]
   Rejected target: [sInstr "swap(a, b);"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cCall "swap" ~args:[[cVar "a"]; [cVar "b"]]];
)
