open Optitrust
open Syntax


let _ = Run.doc_script_cpp (fun _ ->
  !! If_basic.elim_true [occFirst; cIf ()];
  !! If_basic.elim_false [occFirst; cIf ()];
)

"
int main() {
  int x = 0;
  if (true) {
    x++;
  }
  if (false) {
    x--;
  }
}
"

let _ = Run.script_cpp (fun _ ->
  (* TODO: static analysis and expected failures
  !! Trace.failure_expected (
    If_basic.elim_true [nbMulti; cIf ~then_:[cVar "f"] ()];
    If_basic.elim_false [nbMulti; cIf ~then_:[cVar "t"] ()];
  ); *)
  !! If_basic.elim_true [nbMulti; cIf ~then_:[cVar "t"] ()];
  !! If_basic.elim_false [nbMulti; cIf ~then_:[cVar "f"] ()];
)
