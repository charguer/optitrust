open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Matrix.intro_mindex (var "N") [cVarDef "p"];
  
)
