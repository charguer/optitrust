open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Struct.inline "pos" [cTypDef "particle"];
  !!! Struct.inline "speed" [cTypDef "particle"];
  !! Struct.set_explicit [sInstr "b.items[b.nb] = p"];
  !!! Variable.inline ~delete_decl:true [cVarDef "p"];
  !!! Struct.inline "items" [cTypDef "bag"];


)