open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () -> 

   !! Function.uninline ~fct:[cTopFunDef "f"] [cTopFunDef "main"; cVarDef "a"]; 

)

"
void g(int x, int y) {
   int z;
   z = x + y;
}

void f(int x) {
  int a = 10;
  g(a, x);

}

int main() {
  int a = 10;
  g(a, 20);
  return 0;
  
}
"


let _ = Run.script_cpp (fun _ ->

    !! Function.uninline ~fct:[cTopFunDef "f"] [cVarDef "b"];

)
