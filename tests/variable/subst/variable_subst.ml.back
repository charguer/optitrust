open Optitrust
open Target
open Prelude


let _ = Run.doc_script_cpp (fun _ ->
  let a = find_var_in_current_ast "a" in
  !! Variable_basic.subst ~subst:a ~put:(lit "3") [cVarDef "b"];

)

"
int main() {
  int a = (2 + 1);
  int b = a;
}
"

let _ = Run.script_cpp (fun _ ->
  let y = find_var_in_current_ast "y" in
  !! Variable_basic.subst ~subst:y ~put:(expr "2 + x") [cVarDef "z"];
  !! Variable_basic.subst ~subst:y ~put:(Trm.trm_int 5) [cFunDef "main"];

)
