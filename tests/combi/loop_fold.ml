open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.fold ~index:"k" 3 [sInstr "+= 0"];
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


let _ = Run.script_cpp (fun _ ->

  !! Loop.fold  ~index:"k"  3 [cCellWrite ~base:[cVar "values"] ~index:[cInt 0] ()];
)


(* LATER: Loop.fold will compute automatically the difference between the instructions
   in order to guess what should be the index *)