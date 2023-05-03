open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop_basic.slide ~size:(trm_int 3) ~step:(trm_int 1) ~index:"bi" [cFor "i"];
)

"
int main() {
  for (int i = 0; i < 9; i++) {
    int j = i;
  }
}
"

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.slide ~size:(trm_int 2) ~step:(trm_int 1) ~index:"b${id}" [cFor "i"];
  !! Loop_basic.slide ~size:(trm_int 2) ~step:(trm_int 2) [cFor "j"];
)
