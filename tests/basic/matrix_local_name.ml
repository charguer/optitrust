open Optitrust
open Syntax
open Target

let _ = Flags.check_validity := true

let _ = Run.doc_script_cpp (fun _ ->
  let a = find_var_in_current_ast "a" in
  !! Matrix_basic.local_name a ~into:"b" [cFor "i"];

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

  let a = find_var_in_current_ast "a" in
  let b = find_var_in_current_ast "b" in
  !! Matrix_basic.local_name a ~into:"x" [cFor "i"];
  !! Matrix_basic.local_name b ~into:"y" ~alloc_instr:[cWriteVar "b"] [cFor "j"];

)
