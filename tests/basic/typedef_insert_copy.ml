open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Typedef_basic.insert_copy "vect2" [cTypDef "vect"];

)

"
typedef struct {
  int x;
  int y;
} vect;

"

let _ = Run.script_cpp ~parser:Parsers.Clang (fun _ ->

  !! Typedef_basic.insert_copy "vect2" [cTypDef "vect"];

)
