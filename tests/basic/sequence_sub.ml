open Optitrust
open Run

(* TODO: Find out where the second sequence is comming from *)
let _ = run_unit_test (fun _ ->
  Sequence.sub 1 2 [cFunDef "main"; cStrict; cBody];
)
