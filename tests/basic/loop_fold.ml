open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
      !! Loop_basic.fold "i" 0 1 [cLabel "tofold"; dBody];
  )
"
int main() {
  int x;
  tofold: {
    x += 0;
    x += 1;
    x += 2;
  }
}
"

(* TODO: we need labels ~index:"x" ~start:0 ~step:1 for the arguments *)
(* LATER: check that at the combi level, if the target is a labeled expression, automatically add dBody *)



let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.fold  "i" 0 1 [cLabel "iterations"; dBody];
  !! Sequence_basic.intro ~mark:"instrs" 3 [cCellWrite ~base:[cVar "values"] ~index:[cInt 0] ()];
  !! Loop_basic.fold  "k" 0 1 [cMark "instrs"];
)
