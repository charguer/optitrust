open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Arrays_basic.tile "B" ~block_type:"U_BLOCK" [cTypDef "U"];
  !! Arrays_basic.tile "B" [cTypDef "T"];
  !! Arrays_basic.tile "B" [cTypDef "V"];
)
