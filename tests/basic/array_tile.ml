open Optitrust
open Syntax
open Target

let _ = Run.doc_script_cpp (fun _ ->
  let vB = find_var_in_current_ast "B" in
  !! Arrays_basic.tile vB ~block_type:"BLOCK" [cTypDef "T"];

)

"
const int B = 8;
typedef int *T;
int main() {
  T t;
  int a = t[3];
}
"

let _ = Run.script_cpp (fun _ ->
  let vB = find_var_in_current_ast "B" in
  !! Arrays_basic.tile vB ~block_type:"U_BLOCK" [cTypDef "U"];
  !! Arrays_basic.tile vB [cTypDef "T"];
  !! Arrays_basic.tile vB [cTypDef "V"];

)
