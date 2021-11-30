open Optitrust
open Target

(* TODO: another unit test for rename_field *)

let _ = Run.doc_script_cpp (fun _ ->
    !! Struct_basic.rename_fields (fun f -> String.uppercase_ascii f) [cTypDef "obj"];
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

    !! Struct_basic.rename_fields (fun x -> "rel_" ^ x) [cTypDef "vect"];
    !! Struct_basic.(rename_fields Rename.(only_for "pos" (fun x ->  "rel_" ^ x))) [cTypDef "obj"];
    !! Struct_basic.(rename_fields Rename.(only_for "speed" (fun x ->  "rel_" ^ x))) [cTypDef "obj"];
)
