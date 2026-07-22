open Optitrust
open Prelude

(* Difficulty: Level 3
   Request: target the declaration and definition of helper
   Expected target: [cFunDefAndDecl "helper"]
   Accepted variant: [cTopFunDefAndDecl "helper"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFunDefAndDecl "helper"];
)
