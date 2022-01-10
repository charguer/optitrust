open Optitrust
open Target



let _ = Run.doc_script_cpp (fun _ ->
    !! Struct_basic.rename_field "pos" ~into:"POS" [cTypDef "obj"];
  )
"
typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  vect pos;
  int mass;
} obj;
"
let _ = Run.script_cpp (fun _ ->

    !! Struct.rename_field "pos" ~into:"rel_pos" [cTypDef "obj"];
    !! Struct.rename_field "speed" ~into:"rel_speed" [cTypDef "obj"];
)
