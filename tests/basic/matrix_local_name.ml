open Optitrust
open Target



let _ = Run.doc_script_cpp (fun _ ->

  !! Matrix_basic.local_name "a" ~into:"b" [cFor "i"];

)

"
#include \"../../include/optitrust.h\"
typedef int T;
T* b;

int main (){

  const int N0 = 1;
  T* a = (T*) CALLOC1(N0, sizeof(T));
  for (int i = 0; i < 10; i++){
    a[MINDEX1(N0, i)];
  }
}
"

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.local_name  "a" ~into:"x" [cFor "i"];
  !! Matrix_basic.local_name  "b" ~into:"y" ~alloc_instr:[cWriteVar "b"] [cFor "j"];

)
