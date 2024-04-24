open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Matrix.intro_mindex (var "N") [cVarDef "p"];

)
