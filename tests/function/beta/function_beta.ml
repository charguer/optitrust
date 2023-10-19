open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () -> 

  !! Function.beta [cFun "f"];

)

"
void f(int j){
  int s = 0;
  s += 2*j;
  s -= j;
}

int main(){
  int i = 1;
  f(i);
}
"

let _ = Run.script_cpp (fun _ ->

  (* Case when f is a normal function call *)
  !! Function.beta [cFun "f"];

  (* Case when f is an inline function call *)
  !! Variable_basic.inline ~accept_functions:true [cFunDef "g"];
  !! Function.beta ~indepth:true [];

)
