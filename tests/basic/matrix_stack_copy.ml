open Optitrust
open Target
open Path

let _ = Run.doc_script_cpp (fun () ->
   !! Matrix.stack_copy ~var_from:"s" ~var_to:"x" ~fixed_dims:1 [occFirst; cFor "j"];
)

"
#include \"../../include/optitrust.h\"

int main () {
  float* s;
  for (int i = 0; i < 32; i++) {
    // TODO: s[i][j] = x[j]
    for (int j = 0; j < 32; j++) {
      for (int k = 0; k < 4; k++) {
        s[MINDEX2(32, 32, i, j)] += k;
      }
    }
  }
}
"

let _ = Run.script_cpp (fun () ->
   !! Matrix.stack_copy ~var_from:"s" ~var_to:"x" ~fixed_dims:1 [occFirst; cFor "j"];
   !!! ();
)
