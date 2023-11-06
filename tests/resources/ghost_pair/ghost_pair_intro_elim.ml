open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! Ghost_pair.elim [cVarDef "focusA"];
  !! Ghost_pair.intro ~name:"focusA" [cSeq (); cStrict; cCall "matrix1_ro_focus"];
  !! Trace.check_recover_original();

  !! Ghost_pair.elim ~mark_begin:"b" ~mark_end:"e" [cVarDef "focusA"];
  !! Trace.failure_expected (fun () -> Ghost_pair.intro ~name:"focusA" ~end_mark:"other" [cMark "b"]);
  !! Ghost_pair.intro ~name:"focusA" ~end_mark:"e" [cMark "b"];
  !! Trace.check_recover_original();

  !! Ghost_pair.elim ~mark_begin:"Bi" [cVarDef "focusBi"];
  !! Ghost_pair.elim ~mark_begin:"Bj" [cVarDef "focusBj"];
  !! Ghost_pair.intro ~name:"focusBi" [cMark "Bi"];
  !! Ghost_pair.intro ~name:"focusBj" [cMark "Bj"];
  !! Trace.check_recover_original();

  !! Ghost_pair.apply_without_pairs (fun _ -> ()) [cFunDef "main"];
  !! Trace.check_recover_original();

  !! Ghost_pair.elim [nbMulti; cVarDef ~body:[cCall "__ghost_begin"] ""];
)
