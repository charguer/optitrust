open Optitrust
open Target

let _ = Run.script_cpp (fun () ->


  (* After adding function inline transformations *)
  !! Struct.inline "pos" [cTypDef "particle"];
  !!! Struct.inline "speed" [cTypDef "particle"];
  !! Struct.set_explicit [sInstr "b.items[b.nb] = p"];
  (* TODO: Fix the issue with  particle &p = b.items[idParticle];*)
  !!! Variable.inline ~delete_decl:true [cVarDef "p"];
  !!! Struct.inline "items" [cTypDef "bag"];


)