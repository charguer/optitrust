open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.replace_occurrences ~subst:"y" ~put:(expr "(2 + x)") [cVarDef "z"]; 
  !! Variable_basic.replace_occurrences ~subst:"y" ~put:(Ast.trm_int 5) [cFunDef "main"];
)
