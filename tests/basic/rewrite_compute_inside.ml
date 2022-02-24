open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
  !! Rewrite_basic.compute_inside [];
  )
"
#include <stdbool.h>
int f(int x) { return x + (2 + 3); }

int main() {
  int a = f((6 * 3) + 2);
  int c = 1 - 1;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Rewrite_basic.compute_inside [ cLabelBody "block1"];
  !! Rewrite_basic.compute_inside [ cLabelBody "block2"];
  !! Rewrite_basic.compute_inside [ cLabelBody "block3"];

)
