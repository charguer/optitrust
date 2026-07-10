open Optitrust
open Prelude

(* Difficulty: Level 5
   Request: target the j loop that accumulates into sum
   Expected target: [cFor ~body:[cPlusEq ~lhs:[cVar "sum"] ()] "j"]
   Accepted variant: [cFunDef "mm"; cFor ~body:[cPlusEq ~lhs:[cVar "sum"] ()] "j"] *)

let _ = Run.script_cpp (fun () ->
  !! Show.target [cFor ~body:[cPlusEq ~lhs:[cVar "sum"] ()] "j"];
)
