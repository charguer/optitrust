
(* Usage:
      make optitrust && make interact.out
   or
      F6 on a given line, or shift+F6 to recompile optitrust
*)

open Optitrust
open Target


let _ = Run.script_cpp (fun () ->

  !! Label.add "m0" [cVarDef "a"];
  show [cVarDef "a"];
  show [cVarDef "b"];
  show [tFirst; cFunDef "main"; cStrict; dBody];

  (* Show the current ast *)
  show_ast();
  (* Show the resources *)
  Resources.show();

  (* Showing operation with step at front *)
  !! Label.add "m1" [cVarDef "b"];
  show [cVarDef "a"];
  (* Showing operation with reparse *)
  !! Label.add "m2" [cVarDef "a"];
  (* Showing two operations at once *)
  !! Label.add "m3" [cVarDef "a"];
     Label.add "m4" [cVarDef "a"];
  !! Label.add "m5" [cVarDef "a"];

  (* Showing a big-step at once: the diff for each line can be viewed independently,
     but using the right shortcut one views the diff for the two operations at once. *)
  bigstep "a big step";
  !! Label.add "m6" [cVarDef "a"];
  !! Label.add "m7" [cVarDef "a"];

  (* Trace.dump() is called implicitly called at the end of file;
     this function handles the case where the cursor was after the last '!!'. *)
)


