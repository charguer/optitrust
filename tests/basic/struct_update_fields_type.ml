open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
    !! Struct_basic.update_fields_type "x" (ty "float") [cTypDef "vect"];
  )
"
typedef struct {
  int x;
  int y;
} vect;
"

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

  !! Struct_basic.update_fields_type "x" (typ_float ()) [cTypDef "vect"];
  !! Struct_basic.update_fields_type "y" (typ_float ()) [cTypDef "vect"];
)