open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Function.inline [cCall "g"; cCall "f"];
  !! Function.inline [cVarDef "p"; cCall "f"];

)
