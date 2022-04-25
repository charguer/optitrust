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
  
  (* for references *)
  !! Variable_basic.unfold [cVarDef "e"];

  (* for functions *)
  !! Variable_basic.unfold [cFunDef "f"];

  (* failure for non const variables *)
  !! Tools.failure_expected (fun () ->
            Variable_basic.unfold [cVarDef "c"];)
)
