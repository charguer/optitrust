open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  (* for variables *)
  !! Variable_basic.unfold ~at:[cVarDef "b"] [cVarDef "a"];

  (* for functions *)
  !! Variable_basic.unfold [cFunDef "f"]

)
