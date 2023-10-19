open Optitrust
open Target



let _ = Run.doc_script_cpp (fun () -> 
  
  !! Variable.elim_redundant [cVarDef "b"];

)

"
int main(){
  int a = 3;
  int b = 3;
  int c = a + b + 5;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Variable.elim_redundant [cVarDef "b"];
  !! Variable.elim_redundant [cOr [[cVarDef "f"];[cVarDef "g"]]];
  !! Variable.elim_redundant [cVarDef "coef_x2"];

)
