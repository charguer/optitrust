open Optitrust
open Target



let _ = Run.doc_script_cpp (fun _ ->

  !! Variable_basic.ref_to_pointer [cVarDef "b"];

)
"
int main(){

  int a;
  int &b = a;
  b = b + 1;

}
"

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.ref_to_pointer [cVarDef "x"];
  
)
