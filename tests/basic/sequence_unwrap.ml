open Optitrust
open Target

(* Transformation works but there is one issue with the path *)

let _ =
  Run.script_cpp (fun _ ->
    Sequence.unwrap [cSeq ~args:[cVarDef "x"] ()];
    Sequence.unwrap [cSeq ~args:[cVarDef "y"] ()];
  )