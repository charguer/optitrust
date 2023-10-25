
open Optitrust
open Target
(*
let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.fold ~at:[cVarDef "q"] [cVarDef "p"];
  !! Variable_basic.fold ~at:[cVarDef "x"] [cVarDef "p"];

)
