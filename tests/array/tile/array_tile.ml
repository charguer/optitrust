open Optitrust
open Prelude
open Target

let _ = Run.script_cpp (fun _ ->
  let (vB, _) = find_var "B" [] in
  !! Arrays_basic.tile vB ~block_type:"U_BLOCK" [cTypDef "U"];
  !! Arrays_basic.tile vB [cTypDef "T"];
  !! Arrays_basic.tile vB [cTypDef "V"];

)
