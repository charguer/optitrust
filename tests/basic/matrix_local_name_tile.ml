open Optitrust
open Target
open Syntax

let _ = Run.doc_script_cpp (fun _ ->
  !! Matrix_basic.local_name_tile "a" ~into:"b" [(trm_int 3, trm_int 4)] [cFor "i"];
)

"
#include \"../../include/optitrust.h\"

int main (){
  int* a = (int*) CALLOC1(10, sizeof(int));
  for (int i = 3; i < 7; i++) {
    a[MINDEX1(10, i)];
  }
  free(a);
}
"

let _ = Run.script_cpp (fun _ ->
  let tile offset size = (trm_int offset, trm_int size) in
  !! Matrix_basic.local_name_tile "a" ~into:"x" [tile 0 10; tile 2 8; tile 0 4] [cFor ~body:[cArrayWrite "a"] "i"];
  !! Matrix_basic.local_name_tile "b" ~into:"y" [tile 0 10; tile 2 8; tile 0 4] ~alloc_instr:[cWriteVar "b"] [cFor ~body:[cArrayWrite "b"] "j"];
)
