open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable.reuse ~space:"x" [cVarDef "y"];
)
