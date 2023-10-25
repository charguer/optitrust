open Optitrust
open Prelude


let _ = Run.script_cpp (fun _ ->
  !! If_basic.elim_true [occFirst; cIf ()];
  !! If_basic.elim_false [occFirst; cIf ()];
)
