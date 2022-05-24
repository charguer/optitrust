open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
    
  !! Loop_basic.color (lit "2") ~index:"ci" [cFor "i"];

)

"
int main() {
  for (int i = 0; (i < 5); i++) {
  }
}
"

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.color (expr "C") ~index:"ci" [cFor "i"];
  !! Loop_basic.color (expr "C") [cFor "j"];

)
