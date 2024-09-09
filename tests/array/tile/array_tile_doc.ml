open Optitrust
open Prelude
open Target

let _ = Run.script_cpp (fun _ ->
  let vB = find_var "B" [] in
  !! Arrays_basic.tile vB ~block_type:"BLOCK" [cTypDef "T"];

)
