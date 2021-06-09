open Optitrust
open Run


let _ = run_unit_test (fun () ->
  show_target [cVar "a"];
  let p = [cVar "b"] in
  show_target p;
)
