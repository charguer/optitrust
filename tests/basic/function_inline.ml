open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->

  !! Function_basic.inline [cFun "sq"];

)

"
int sq(int x) { return (x * x); }

int main() {
  int r = sq(3);
}
"

let _ = Run.script_cpp (fun _ ->

  let tf = cTopFunDef "test_fun" in 
  !! Function_basic.inline ~body_mark:"bodyf" [tf;cFun "f"];
  !! Function_basic.inline [tf;cFun "g"];
  !! Function_basic.inline ~body_mark:"bodyh" [tf;cFun "h"];
  !! Function_basic.inline [tf;cFun "m"];
  !! Function_basic.inline ~body_mark:"bodyk" [tf;cFun "k"];

  let tc = cTopFunDef "test_class_method" in 
  
  !! Function_basic.inline ~body_mark:"bodyf" [tc;cFun "f"];
  !! Function_basic.inline [tf;cFun "g"];
  !! Function_basic.inline ~body_mark:"bodyh" [tf;cFun "h"];
  !! Function_basic.inline [tf;cFun "m"];
  !! Function_basic.inline ~body_mark:"bodyk" [tf;cFun "k"];

)
