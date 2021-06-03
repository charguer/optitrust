open Optitrust

let _ = run_unit_test (fun _ ->
  Optitrust__Sequence.sub 1 2 [cFun "main"; cBody()];
  