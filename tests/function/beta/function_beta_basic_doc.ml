open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.inline [cFunDef "sq"];
  !! Function_basic.beta [cVarDef "r"; cCall ""];

)
