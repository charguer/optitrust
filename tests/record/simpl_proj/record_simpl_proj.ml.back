open Optitrust
open Target

let _ = Run.doc_script_cpp ~parser:CParsers.menhir (fun _ ->

  !! Record_basic.simpl_proj [cVarDefs ["a"; "b"]];

)
"
typedef struct {
  int x;
  int y;
} vect;

int main() {
  int a = (vect){1, 2}.x;
  int b = (vect){1, 2}.y;
}
"

let _ = Run.script_cpp ~parser:CParsers.menhir (fun _ ->

  !! Record_basic.simpl_proj [cFunDef "main"];

)
