open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () -> 

  (* (); *)
  !! Struct.align_field (lit "16") "." [cTypDef "vect"];

)
"
typedef struct { 
  int x;
  int y;
} vect;

int main() {}
"



let _ = Run.script_cpp (fun () ->


   !! Struct.align_field (lit "16") "items." [cTypDef "chunk"];

)