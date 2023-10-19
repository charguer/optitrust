open Optitrust
open Target
open Prelude


let _ = Run.script_cpp (fun _ ->
  let a = find_var_in_current_ast "a" in
  !! Variable_basic.subst ~subst:a ~put:(lit "3") [cVarDef "b"];

)
