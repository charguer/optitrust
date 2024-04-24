open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! Ghost_pair.elim ~mark_begin:"b" ~mark_end:"e" [cVarDef "pair"];
  !! Trace.failure_expected (fun _e -> true) (fun () -> Ghost_pair.intro ~end_mark:"other" [cMark "b"]);
  !! Ghost_pair.intro ~name:"pair" ~end_mark:"e" [cMark "b"];
  !! Trace.check_recover_original();

  !! Ghost_pair.elim [cVarDef "pair"];
)

