open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->

    !! Function_basic.dsp_call [cFun "f"];
)

"
int f(int x);
void f_dsp(int x, int* r);

int main() {
  int r;
  int x = 10;
  r = f(x);
  return 0;
}
"

let _ = Run.script_cpp (fun _ ->

    !! Function_basic.dsp_call [cFun "f"];
    !! Function_basic.dsp_call [cFun "g"];
    !! Function_basic.dsp_call ~dsp:"my_h" [cFun "h"];
)
