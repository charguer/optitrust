open Optitrust
open Syntax

let _ = Run.doc_script_cpp (fun _ ->
  let x = find_var_in_current_ast "x" in
  !! Variable_basic.local_name x ~into:"y" [cLabel "sec"];

)

"
int main() {
  int x = 0;
sec:{
  x = x + 1;
  x = x + 2;
}
  int r = x;
}

"

let _ = Run.script_cpp (fun _ ->
  let a = find_var_in_current_ast "a" in
  !! Variable_basic.local_name ~mark:"mymark" a  ~into:"x" [cFor "i"];

)
