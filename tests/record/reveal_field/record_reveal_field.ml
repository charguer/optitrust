open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Record_basic.reveal_field "pos" [cTypDef "particle"];
  !! Record_basic.reveal_field "speed" [cTypDef "particle"];
  !! Record_basic.reveal_field "items" [cTypDef "chunk"];

)

(* LATER: at the combi level, combine struct_inline with struct-renaming-field *)
