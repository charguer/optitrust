open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->
  let a = find_var_in_current_ast "a" in
  !! Variable_basic.subst ~reparse:true ~subst:a ~put:(lit "3") [cVarDef "b"];

)
