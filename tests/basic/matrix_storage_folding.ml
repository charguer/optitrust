open Optitrust
open Ast
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Matrix_basic.storage_folding ~var:"a" ~dim:0 ~n:(trm_int 3) [cFunBody "main"];
)

"
#include \"../../include/optitrust.h\"

int main() {
  int* a = (int*) MALLOC1(10, sizeof(int));
  int* b = (int*) MALLOC1(8, sizeof(int));
  for (int i = 0; i < 10; i++) {
     a[MINDEX1(10, i)] = i;
    if (i >= 2) {
      b[MINDEX1(8, i - 2)] = a[MINDEX1(10, i - 2)] +
                             a[MINDEX1(10, i - 1)] +
                             a[MINDEX1(10, i)];
    }
  } 

  return 0;
}
"

let _ = Run.script_cpp (fun _ ->
  !! Matrix_basic.storage_folding ~var:"a" ~dim:0 ~n:(trm_int 3) [cFunBody "main"];
  !! Matrix_basic.storage_folding ~var:"b" ~dim:0 ~n:(trm_int 3) [cFunBody "main"];
  (* TODO?
  !! Matrix_basic.storage_folding ~storage:Variables *)
)