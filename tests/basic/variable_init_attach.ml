open Optitrust
open Target

(* TODO: missing unit test for init_detach; similar to here *)



let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.init_attach [cVarDef "a"];
  )
"
int main() {
  int a;
  a = 1;
  int b = 2;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.init_attach [cVarDef "x"];
)
