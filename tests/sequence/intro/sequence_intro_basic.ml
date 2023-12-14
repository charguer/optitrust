open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cFor "i"];
  !! Sequence_basic.intro_between [tBefore; cVarDef "y"] [tAfter; cVarDef "t"];
  !! Sequence_basic.intro_after [cVarDef "u"];
  !! Trace.alternative (fun _ ->
      Sequence_basic.intro_before [cVarDef "u"];
      !! ();
  );

  !! Trace.failure_expected (fun _e -> true) (fun () ->
       Sequence_basic.intro_between [tAfter; cVarDef "z"] [tBefore; cVarDef "z"]);
  !! Trace.failure_expected (fun _e -> true) (fun () ->
       Sequence_basic.intro_between [tAfter; cVarDef "z"] [tAfter; cVarDef "z"]);

  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Sequence_basic.intro 1 [cFor "k"; cVarDef "a"]);
  !! Sequence_basic.intro 2 [cFor "k"; cVarDef "b"];

  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Sequence_basic.intro 1 [cFor "l"; cVarDef "a"]);
)
