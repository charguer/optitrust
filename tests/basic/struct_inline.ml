open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Struct_basic.inline "pos" [cTypDef "obj"];
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

   !! Struct_basic.inline "pos" [cTypDef "particle"];
   !! Struct_basic.inline "speed" [cTypDef "particle"];
   !! Struct_basic.inline "items" [cTypDef "chunk"];
)

(* LATER: at the combi level, combine struct_inline with struct-renaming-field *)
