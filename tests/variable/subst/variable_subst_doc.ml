open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->
  let a = find_var "a" [] in
  !! Variable_basic.subst ~reparse:true ~subst:a ~put:(lit "3") [cVarDef "b"];

)
