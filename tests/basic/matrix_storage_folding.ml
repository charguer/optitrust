open Optitrust
open Ast
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Matrix_basic.storage_folding ~var:"a" ~dim:0 ~size:(trm_int 3) [cFunBody "main"];
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
  (* TODO LATER: what about record field x.m:
      & a = x.m
      deal with alloc assign without let

      COMBI target alloc instead of ~var
       + may introduce variable for basic
     *)
  !! Matrix_basic.storage_folding ~var:"a" ~dim:0 ~size:(trm_int 3) [cFunBody "main"];
  !! Matrix_basic.storage_folding ~var:"b" ~dim:0 ~size:(trm_int 3) [cFunBody "main"];
  (* TODO?
  Array.to_variables
   + generalize to Matrix.to_variables on last dim
  !! Matrix_basic.storage_folding ~storage:Variables
  + unroll and avoid rotations
    - either circular buffer + unroll + simpl;
    - or
  //
  !! Loop.rotate_values ~i
  + prelude + loop
  *)
)