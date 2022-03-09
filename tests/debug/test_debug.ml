open Optitrust
open Target

let _ = Run.script_cpp ~parser:Parsers.Menhir (fun _ ->

   !! Struct_basic.inline "pos" [cTypDef "particle"];
   !! Struct_basic.inline "speed" [cTypDef "particle"];
   !! Struct_basic.inline "items" [cTypDef "chunk"];
)

(* LATER: at the combi level, combine struct_inline with struct-renaming-field *)
