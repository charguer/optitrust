open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Matrix.intro_mops (trm_var "N") [cVarDef "p"]; (* TODO: the read operation is missed *)
  !! Matrix.intro_mops (trm_var "N") [cVarDef "q"];
)
