open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Sequence.unwrap [cSeq ~args:[cVarDef "x"] ()];
    !! Sequence.unwrap [cSeq ~args:[cVarDef "y"] ()];
)
