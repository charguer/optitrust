open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  
  !! Struct_basic.inline "pos" [cTypDef "particle"];
  !! Struct_basic.inline "speed" [cTypDef "particle"];
  !! Struct_basic.set_explicit [cTopFun "bag_push"; sInstr "= p"];
  !! Variable_basic.inline ~delete:true [cVarDef "p"];
  !!! Struct_basic.inline "items" [cTypDef "bag"];
  
  

)