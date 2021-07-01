open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Struct.inline "pos" [cTypDef "particle"];
  !!! Struct.inline "speed" [cTypDef "particle"];
  !!! Struct.inline "items" [cTypDef "bag"];

)