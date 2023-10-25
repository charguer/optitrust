open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Matrix_basic.reorder_dims ~order:[1;0] [cFun "CALLOC2"];
  !! Matrix_basic.reorder_dims ~order:[1;0] [cFun "MINDEX2"];

)

"
#include \"../../../include/optitrust.h\"

int main () {
  int i1 = 0, i2 = 0;
  const int N1 = 10;
  const int N2 = 10;
  int* p = (int*) CALLOC2(N1, N2, sizeof(int));
  p[MINDEX2(N1,N2,i1,i2)];
  return 0;
}
"

let _ = Run.script_cpp ~parser:CParsers.clang (fun _ ->

  !! Matrix_basic.reorder_dims ~order:[2;1;0] [cFun "CALLOC3"];
  !! Matrix_basic.reorder_dims ~rotate_n:2 [cFun "MINDEX3"];

)
