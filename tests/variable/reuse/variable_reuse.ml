open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Variable.reuse (var "x") [cVarDef "y"];
)
