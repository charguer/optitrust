open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
  () (* TODO *)
)

"
int main() {
  for (int i = 0; i < 9; i++) {
    int j = i;
  }
}
"

let _ = Run.script_cpp (fun _ ->
  !! Loop.slides ~size_steps:[
    Some (trm_int 2, trm_int 1);
    Some (trm_int 3, trm_int 2);
  ] [cFor "i"];
)
