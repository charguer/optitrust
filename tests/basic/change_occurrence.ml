open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Generic.change_occurrence "b" [nbMulti;cVar "a"];
  !! Generic.change_occurrence "f1" [cFun "f"; cVar "f"];

)