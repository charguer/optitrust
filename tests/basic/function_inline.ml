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

  !! Function_basic.inline ~body_mark:"bodyf" [cFun "f"];
  !! Function_basic.inline [cFun "g"];
  !! Function_basic.inline ~body_mark:"bodyh" [cFun "h"];
  !! Function_basic.inline [cFun "m"];
  !! Function_basic.inline ~body_mark:"bodyk" [cFun "k"];

)
