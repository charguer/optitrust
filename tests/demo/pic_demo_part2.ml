open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Struct.inline "pos" [cTypDef "particle"];
  !! Struct.inline "speed" [cTypDef "particle"];
  !! Struct.set_explicit [cTopFun "bag_push"; sInstr "= p"];
  !! Variable.inline ~delete_decl:true [cVarDef "p"];
  !!! Struct.inline "items" [cTypDef "bag"];


)