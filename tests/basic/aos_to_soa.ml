open Optitrust
open Run

(* TODO: Fix the issue with get_typedef *)
let _ = run_unit_test (fun () ->
  Arrays.aos_to_soa [cTypDef "vects"];
)



