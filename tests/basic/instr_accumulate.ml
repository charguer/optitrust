open Optitrust
open Target



let _ = Run.doc_script_cpp (fun _ ->
    !! Instr_basic.accumulate [cLabelBody "fuse"];
  )
"
int main() {
  int x = 0;
  fuse: {
    x += 1;
    x += 2;
    x += 3;
  }
}
"
(* TODO: if the first operation has an annotation "infix_op", ie is of the form x+=..
   then the output should be  x += .. instead of x = x + ..
   (that is, we would preserve the infix_op annotation) *)

let _ = Run.script_cpp (fun _ ->

  !! Instr_basic.accumulate [cLabelBody "test1"];
  !! Instr_basic.accumulate [cLabelBody "test2"];


)
