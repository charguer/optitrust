open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Variable_basic.to_nonconst [cVarDef "x"];

)

"
int main() {
  const int x = 3;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.to_nonconst [cVarDef "x"];
  !! Variable_basic.to_nonconst [cVarDef "y"];
  !! Variable_basic.to_nonconst [cVarDef "z"];

)
