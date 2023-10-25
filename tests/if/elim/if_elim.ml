open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->
  (* TODO: static analysis and expected failures
  !! Trace.failure_expected (
    If_basic.elim_true [nbMulti; cIf ~then_:[cVar "f"] ()];
    If_basic.elim_false [nbMulti; cIf ~then_:[cVar "t"] ()];
  ); *)
  !! If_basic.elim_true [nbMulti; cIf ~then_:[cVar "t"] ()];
  !! If_basic.elim_false [nbMulti; cIf ~then_:[cVar "f"] ()];
)
