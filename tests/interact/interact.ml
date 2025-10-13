
(* Usage:
      make optitrust && make interact.out
   or
      F6 on a given line to view a diff
      shift+F5 for compiling a full trace
*)

open Optitrust
open Target
open Prelude

let _ = Run.script_cpp (fun () ->

  !! Label.add "m0" [cVarDef "a"];
  (* TODO: fix show term--
  ShowAt.trm [cVarDef "a"];
  ShowAt.trm [cVarDef "b"];
  ShowAt.trm [tFirst; cFunBody "main"]; *)


  (* FOR TESTING: uncomment to test an incorrect path:
  let ps = Target.resolve_target [cVarDef "a"] in
  !! Label.add "wrong" (target_of_path ((List.hd ps) @ Path.[Dir_else]));
  *)

  (* FOR TESTING: uncomment to test an incorrect transformation:
  !! Variable.init_detach [cVarInit "a"];
  *)

  (* Showing operation with step at front *)
  !! Label.add "m1" [cVarDef "b"];
  (* TODO: fix show term-- ShowAt.trm [cVarDef "a"]; *)
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

  (* Trace.dump is called implicitly called at the end of file *)
)


