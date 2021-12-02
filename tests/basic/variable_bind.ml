open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.bind "b"  [cFun "f"];
  )
"
int f(int x);
int g(int x);

int main() {
  int a = 1;
  int r = g(f(a + 3));
}
"

(* TODO: This generates non valid C code, but this transformation it's mostly used in intermediate steps hence
    the final code will be okday

 *)
let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.bind "a" ~const:true [cFunDef "test"; cReturn;cArrayInit];
  !! Variable_basic.bind "b"  [cVarDef "x"; cArrayInit];
)
