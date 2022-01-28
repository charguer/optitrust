open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.fold ~index:"k" 4 [sInstr "a += 0"];
)
"
int main() {
  int a = 0;
  a += 0;
  a += 1;
  a += 2;
  a += 3;
}
"

(* TODO: why does this one not work?
Because the correct implementation works only for start = 0
let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.fold ~index:"k" ~start:1 3 [sInstr "a += 1"];
)
"
int main() {
  int a = 0;
  a += 1;
  a += 2;
  a += 3;
}
"
*)

let _ = Run.script_cpp (fun _ ->

  !! Loop.fold  ~index:"k"  3 [cCellWrite ~base:[cVar "values"] ~index:[cInt 0] ()];
)


(* LATER: Loop.fold will compute automatically the difference between the instructions
   in order to guess what should be the index *)