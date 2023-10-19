open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Matrix_basic.intro_calloc [nbMulti; cFun "calloc"];

)

"
#include <stdlib.h>
#include \"../../../include/optitrust.h\"
int main () {
  const int N = 10;
  int* p = (int*) calloc(N, sizeof(int));
}
"



let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.intro_calloc [nbMulti; cFun "calloc"];

)
