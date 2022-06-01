open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () -> 

  !! Function.inline [cFun "f"];

)

"
typedef struct { 
  int x;
  int y;
} vect;

vect f(int a){
  return { a-1, a};
}

int main(){
  int p = f(3).x;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Function.inline ~resname:"r" [cFun "g"; cFun "f"];
  !! Function.inline ~resname:"r" [cVarDef "p"; cFun "f"];

)
