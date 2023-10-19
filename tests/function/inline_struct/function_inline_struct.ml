open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Function.inline ~resname:"r" [cFun "g"; cFun "f"];
  !! Function.inline ~resname:"r" [cVarDef "p"; cFun "f"];

)
