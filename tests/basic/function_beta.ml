open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->

     Variable_basic.inline ~accept_functions:true [cFunDef "sq"];
  !! Function_basic.beta [cVarDef "r"; cFun ""];

)

"
int sq(int x) { return (x * x); }

int main() {
  int r = sq(3);
}
"

let _ = Run.script_cpp (fun _ ->
  (* Functions *)
  !! Variable_basic.unfold ~accept_functions:true [cTopFunDef "f"];
  !! Function_basic.beta [cTopFunDef "test_fun"; cFun ""];

  (* Class methods *)
  !! Variable_basic.unfold ~accept_functions:true [cFunDef "f_X"];
  !! Function_basic.beta [cTopFunDef "test_method"; cFun ""];
)
