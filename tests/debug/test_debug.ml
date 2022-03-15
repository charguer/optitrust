open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   !! Struct.update_fields_type "itemsPos." (atyp "float") [cTypDef "chunk"];
   
)

(* LATER: at the combi level, combine struct_inline with struct-renaming-field *)
