open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () ->

  !! Function.elim_body ~vars:(AddSuffix "_1") [cLabel "body"];

)

"
int main (){
  int x = 3;
  int y;
  body:{
    int a = x + x;
    y = (a + a);
  }
  return 0;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Function.elim_body ~vars:(AddSuffix "_1") [cLabel "body1"];
  !! Function.elim_body [cLabel "body2"];
  !! Function.elim_body ~vars:(AddSuffix "_2") [cLabel "body3"];
  !! Function.elim_body [cLabel "body4"];

)
