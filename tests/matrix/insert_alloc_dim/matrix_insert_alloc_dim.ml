open Optitrust
open Prelude


let _ = Run.doc_script_cpp (fun _ ->

  !! Matrix_basic.insert_alloc_dim (expr "N2") [cMalloc ()];

)

"
#include <stdlib.h>

#include \"../../include/optitrust.h\"

int main() {
  const int N1 = 10;
  const int N2 = 20;
  int *p = (int *)MALLOC1(N1, sizeof(int));
  return 0;
}
"

let _ = Run.script_cpp (fun () ->

  !! Matrix_basic.insert_alloc_dim (expr "N2") [cMalloc ()];
  !! Matrix_basic.insert_alloc_dim (expr "N2") [cCalloc ()];

)
