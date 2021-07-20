open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  show [cVarDef "x"];
  !!Loop.invariant [cVarDef "x"];
)
