open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->
  let y = find_var "y" [] in
  !! Variable_basic.subst ~subst:y ~put:(expr "2 + x") [cVarDef "z"];
  !! Variable_basic.subst ~reparse:true ~subst:y ~put:(Trm.trm_int 5) [cFunDef "main"];

)
