open Optitrust
open Target

(* LATER: wait until we have support for templates to properly implementation CHOOSE
   and document this function. *)

let _ = Run.doc_script_cpp (fun () ->

  !! Specialize_basic.choose "a" [cChoose];

)

"
typedef int T;

T CHOOSE (int nb, T a, T b) {return a;}

int main() {
  T a = 0;
  T b = 1;
  T c;
  c = CHOOSE(2, a, b);
  return 0;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Specialize_basic.choose "xa" [cChoose];

)
