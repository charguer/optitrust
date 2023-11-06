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

  !! Target.iter (fun _ p ->
        Marks.with_marks (fun gen_mark ->
          let pairs = Ghost_pair.elim_all_pairs_at gen_mark p in
          Ghost_pair.reintro_pairs_at pairs p
        )
      ) [cFunDef "main"];
  !! Trace.check_recover_original();

  !! Ghost_pair.elim [nbMulti; cVarDef ~body:[cCall "__ghost_begin"] ""];
)
