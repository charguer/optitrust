open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

    !! Record_basic.rename_fields (fun f -> String.uppercase_ascii f) [cTypDef "obj"];
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

  !! Record_basic.rename_fields (fun x -> "rel_" ^ x) [cTypDef "vect"];
  !! Record_basic.(rename_fields Rename.(only_for "pos" (fun x ->  "rel_" ^ x))) [cTypDef "obj"];
  !! Record_basic.(rename_fields Rename.(only_for "speed" (fun x ->  "rel_" ^ x))) [cTypDef "obj"];

  !! Record_basic.rename_fields (fun x -> "new_" ^ x) [cTypDef "TestFieldRename"];

)
