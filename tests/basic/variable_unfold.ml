open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Variable_basic.unfold ~at:[cVarDef "r"] [cVarDef "a"];
  )
"
int main() {
  const int a = 3;
  const int b = 4;
  int r = a + a + b;
}
"

let _ = Run.script_cpp (fun _ ->

  (* for variables *)
  !! Variable_basic.unfold ~at:[cVarDef "b"] [cVarDef "a"];

  (* for functions *)
  !! Variable_basic.unfold [cFunDef "f"]

)
