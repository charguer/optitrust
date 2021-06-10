open Optitrust
open Target

(* Works *)
let _ = Run.script_cpp (fun _ ->
  Sequence.sub 1 2 [cFunDef "main"; cStrict; cBody];
)
