open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
  !! Variable.reuse ~space:(var "x") [cVarDef "y"];
  )
"
int main() {
  int x = 0;
  x = x + 1;
  int y = 2;
  y = y + 3;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Variable.reuse ~space:(var "x") [cVarDef "y"];

)
