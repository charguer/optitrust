open Optitrust
open Target

(* TODO: there are some marks left over, why? *)

(* TODO:  If we split in a sequence that is the body of a function we get something that seems to break
   our C invariants, because the function body should always have one proper trm_seq as argument.
   I think that if we split at a path of the form   ... :: Dir_fun_body :: Dir_seq_nth n
   then we should get in the function body a sequence that itself contains two subsequences.

   int main() {
    int a = 0;
    int b = 0;
    int c = 0;
    int d = 0;
   }
    *)

let _ = Run.doc_script_cpp (fun _ ->
  !! Sequence_basic.split [tAfter; sInstr "b = 0"];
  )
"
int main() {
  {
    int a = 0;
    int b = 0;
    int c = 0;
    int d = 0;
  }
}
"


let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.split [tAfter; sInstr "x += y"]; (* TODO: fix this *)
)