open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    
  !! Variable_basic.insert ~const:true "a" "int" "300" [ tAfter; cTypDef "vect"];
  !! Variable_basic.insert "b" "int" "500" [ tAfter; cTypDef "vect"];
)
