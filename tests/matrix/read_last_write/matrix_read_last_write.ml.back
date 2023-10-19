open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Matrix_basic.read_last_write ~write:[cArrayWrite "t"] [cArrayRead "t"];
)

"
#include \"../../../include/optitrust.h\"

int main() {
  int* t = (int*) MALLOC1(2, sizeof(int));
  for (int i = 0; i < 2; i++) {
    t[MINDEX1(2, i)] = i;
  }
  for (int i = 0; i < 2; i++) {
    int l = t[MINDEX1(2, i)];
  }
}
"

let _ = Run.script_cpp (fun _->
  show [cArrayWrite "t"];
  !! Matrix_basic.read_last_write ~write:[cArrayWrite "t"] [cArrayRead "t"];
  !! Matrix_basic.read_last_write ~write:[cArrayWrite "t2"] [nbMulti; cArrayRead "t2"];
  !! Matrix_basic.read_last_write ~write:[cArrayWrite "t3"] [nbMulti; cArrayRead "t3"];
  !! Matrix_basic.read_last_write ~write:[cArrayWrite "img"] [cArrayRead "img"];
)
