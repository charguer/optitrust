open Optitrust
open Prelude

(* Difficulty: Level 2
   Request: target the call to foo inside main
   Expected target: [cTopFunDef "main"; cCall "foo"]
   Accepted variant: [cFunBody "main"; cCall "foo"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cTopFunDef "main"; cCall "foo"];
)
