open Optitrust
open Target
open Prelude

let _ = Run.doc_script_cpp (fun _ ->

    !! Record_basic.update_fields_type "x" (ty "float") [cTypDef "vect"];

)

"
typedef struct {
  int x;
  int y;
} vect;
"

let _ = Run.script_cpp (fun _ ->

  !! Record_basic.update_fields_type "x" (typ_float ()) [cTypDef "vect"];
  !! Record_basic.update_fields_type "y" (typ_float ()) [cTypDef "vect"];

)
