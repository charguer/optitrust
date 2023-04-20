open Optitrust
open Ast
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Matrix_basic.delete ~var:"a" [cFunBody "main"];
)

"
#include \"../../include/optitrust.h\"

int main() {
  int* a = (int*) MALLOC1(10, sizeof(int));
  free(a);
  return 0;
}
"

let _ = Run.script_cpp (fun _ ->
  !! Matrix_basic.delete ~var:"a" [cFunBody "main"];
  !! Matrix_basic.delete ~var:"b" [cFunBody "main"];
)