open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Function.inline ~resname:"r" [cCall "g"; cCall "f"];
  !! Function.inline ~resname:"r" [cVarDef "p"; cCall "f"];

)
