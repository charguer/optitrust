open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.doc_script_cpp (fun _ ->
  let a = find_var_in_current_ast "a" in
  !! Matrix_basic.delete ~var:a [cFunBody "main"];
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
  let a = find_var_in_current_ast "a" in
  let b = find_var_in_current_ast "b" in
  !! Matrix_basic.delete ~var:a [cFunBody "main"];
  !! Matrix_basic.delete ~var:b [cFunBody "main"];
)