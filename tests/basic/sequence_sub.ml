open Optitrust
open Run

(* Works *)
let _ = run_unit_test (fun _ ->
  Sequence.sub 1 2 [cFunDef "main"; cStrict; cBody];
)
