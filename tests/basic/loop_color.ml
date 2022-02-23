open Optitrust
open Target

(* let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.color "2" ~index:"ci" [cFor "i"];
  )
"
int main() {
  for (int i = 0; (i < 5); i++) {
  }
}
" *)

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.color "C" ~index:"ci" [cFor "i"];
  !! Loop_basic.color "C" [cFor "j"];
)
