open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->

    !! Struct_basic.reveal_field "pos" [cTypDef "obj"];

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

   !! Struct_basic.reveal_field "pos" [cTypDef "particle"];
   !! Struct_basic.reveal_field "speed" [cTypDef "particle"];
   !! Struct_basic.reveal_field "items" [cTypDef "chunk"];

)

(* LATER: at the combi level, combine struct_inline with struct-renaming-field *)
