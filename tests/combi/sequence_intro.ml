open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Sequence.intro ~on:[cVarDef "a"] ();
  !! Sequence.intro ~start:[tBefore; cVarDef "b"] ~stop:[tAfter; cVarDef "c"] ();
  !! Sequence.intro ~start:[cVarDef "d"] ~nb:2 ();
  !! Sequence.intro ~stop:[cVarDef "g"] ~nb:2 ();

  !! Tools.failure_expected (fun () ->
       Sequence.intro ~start:[tAfter; cVarDef "z"] ~stop:[tBefore; cVarDef "z"] ());
  !! Tools.failure_expected (fun () ->
       Sequence.intro ~start:[tAfter; cVarDef "z"] ~stop:[tAfter; cVarDef "z"] ());
)
