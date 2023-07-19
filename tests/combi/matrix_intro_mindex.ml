open Optitrust
open Target
open Syntax


let _ = Run.doc_script_cpp (fun () ->

  !! Matrix.intro_mindex (var "N") [cVarDef "p"];

)

"
int main(){
  int p[5] = {0,1,2,3,4};
  for (int i = 1; i < 5; i++){
    int a = p[i-1] + 2;
    p[i] = a;
  }

}
"

let _ = Run.script_cpp (fun _ ->

  !! Matrix.intro_mindex (var "N") [cVarDef "p"];

)
