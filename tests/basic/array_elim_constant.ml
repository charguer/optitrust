open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Arrays_basic.elim_constant [cVarDef "t"];

)

"
int main() {
  const int t[2] = { 0, 1 };
  int a = t[0];
}
"

let _ = Run.script_cpp (fun _ ->

  !! Arrays_basic.elim_constant [cVarDef "t"];

)
