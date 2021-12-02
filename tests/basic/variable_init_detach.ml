open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Variable_basic.init_detach [cVarDef "a"];

)
"
int main() {
  int a = 1;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.init_detach [cVarDef "x"];
  !! Variable_basic.init_detach [cVarDef "y"];

)
