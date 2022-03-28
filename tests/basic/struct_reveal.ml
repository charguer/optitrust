open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Struct_basic.reveal "pos" [cTypDef "obj"];
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

   !! Struct_basic.reveal "pos" [cTypDef "particle"];
   !! Struct_basic.reveal "speed" [cTypDef "particle"];
   !! Struct_basic.reveal "items" [cTypDef "chunk"];
)

(* LATER: at the combi level, combine struct_inline with struct-renaming-field *)
