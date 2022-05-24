open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
    !! Function_basic.use_infix_ops_at [cWriteVar "x"];
)

"
int main() {
  int x = 2;
  x = x + 1;
}
"

let _ = Run.script_cpp (fun _ ->

    !! Function_basic.use_infix_ops_at [nbMulti; cWriteVar "x"];
)

(* ARTHUR: add an efficient mechanism for targeting all potential infix ops in depth *)
