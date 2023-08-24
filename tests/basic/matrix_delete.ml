open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.doc_script_cpp (fun _ ->
  !! Matrix_basic.delete ~var:"a" [cFunBody "main"];
)

"
#include \"../../include/optitrust.h\"

int main() {
  int* const a = (int* const) MALLOC1(10, sizeof(int));
  MFREE1(10, a);
  return 0;
}
"

let _ = Run.script_cpp (fun _ ->
  !! Matrix_basic.delete ~var:"a" [cFunBody "main"];
  !! Matrix_basic.delete ~var:"b" [cFunBody "main"];
)