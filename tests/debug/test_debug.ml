open Optitrust
open Target



let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   !! Struct_basic.inline "pos" [cTypDef "particle"];
   !! Struct_basic.inline "speed" [cTypDef "particle"];
   !! Struct_basic.inline "particle" [cTypDef "chunk"];
)
