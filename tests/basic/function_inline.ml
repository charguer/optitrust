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

(* TODO: by default, when ~body_mark is empty, no mark should be left visible after the transformation *)


let _ = Run.script_cpp (fun _ ->

  !! Function_basic.inline ~body_mark:"bodyf" [cFun "f"];
  !! Function_basic.inline ~body_mark:"bodyg" [cFun "g"];
  !! Function_basic.inline ~body_mark:"bodyh" [cFun "h"];
  !! Function_basic.inline ~body_mark:"bodym" [cFun "m"];
  !! Function_basic.inline ~body_mark:"bodyk" [cFun "k"];
)


