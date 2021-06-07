open Optitrust
open Run

(* TODO: rename this file to sequence_inline.ml *)

let _ =
  run_unit_test (fun _ ->
    (* TODO: Does not work correctly *)
    Sequence.inline 1 [cSeq ~args:[cVarDef "y"]];
  )