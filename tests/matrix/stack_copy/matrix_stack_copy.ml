open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.doc_script_cpp (fun () ->
  !! ();
  (* FIXME???
  let s = find_var_in_current_ast "s" in
  !! Matrix_basic.stack_copy ~var:s ~copy_var:"x" ~copy_dims:1 [occFirst; cFor "j"]; *)
)

"
#include \"../../../include/optitrust.h\"

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
  let s = find_var_in_current_ast "s" in
   !! Matrix_basic.stack_copy ~var:s ~copy_var:"x" ~copy_dims:1 [occFirst; cFor "j"];
   !!! ();
)
