open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->

  !! Matrix.intro_mops (var "N") [cVarDef "p"];
  !! Matrix.intro_mops (var "N") [cVarDef "q"];

)
