open Optitrust
open Target

(* TODO: to discuss:
   the module sequence_core should only implement the intro function
   that takes the index of the start and the nb of items to group.
   It should not include intro_after, before, between, etc..
   which should all be expressed in terms of sequence_basic.intro,
   or possibly sequence.intro at the combi level. *)

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.intro 2 [cFor "i"];
  !! Sequence_basic.intro_between [tBefore; cVarDef "y"] [tAfter; cVarDef "t"];
  !! Sequence_basic.intro_after [cVarDef "u"];
  !! Trace.alternative (fun _ ->
      Sequence_basic.intro_before [cVarDef "u"];
      !! ();
  );

  !! Tools.failure_expected (fun () ->
       Sequence_basic.intro_between [tAfter; cVarDef "z"] [tBefore; cVarDef "z"]);
  !! Tools.failure_expected (fun () ->
       Sequence_basic.intro_between [tAfter; cVarDef "z"] [tAfter; cVarDef "z"]);
)
