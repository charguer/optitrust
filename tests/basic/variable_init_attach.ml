open Optitrust
open Target


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
  !! Tools.failure_expected (fun _ ->
      Variable_basic.init_attach [cVarDef "z"])

)
