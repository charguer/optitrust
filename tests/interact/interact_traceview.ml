open Optitrust
open Prelude

(* Uncomment the line below to see encoded syntax in trace
let _ = Flags.print_optitrust_syntax := true
*)
let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  (* Try task "view trace" on this program; debug messages below should appead in the trace *)
  bigstep "first part";
  !! Label.add "lab1" [cVarDef "a"];
  !! Label.add "lab2" [cVarDef "a"];
  bigstep "second part";
  (* Uncomment the line below to see a partial trace
    !! if true then failwith "the error message";
  *)
  !! Label.add "lab3" [cVarDef "a"];
  (* Try task "View diff" on the line below; debug messages below should appear on stdout *)
  !! Trace.msg "debug message before\nlab4 \n";
      Label.add "lab4" [cVarDef "a"];
      Trace.msg "debug message after\nlab4 \n";
  !! Label.add "lab5" [cVarDef "a"];
  (* Examples of show functions with output in browser *)
  !! Show.ast ();
  !! Resources.ensure_computed ();
  !! Show.res ();
  !! Show.ctx ();
  !! Show.target [cVarDef ""];
  (* Example with two show in a same small-step *)
  !! Show.ast (); Show.ast ();
  (* Examples of show functions with output on stdout *)
  (* !! ShowAt.trm []; *)
  bigstep "third part";
  (* Try task "View diff using internal syntax" *)
  !! Label.add "lab6" [cVarDef "a"];
  !! Label.add "lab7" [cVarDef "a"];
  (* Example of backtracking *)
  !! ignore (Trace.step_backtrack (fun () ->
       Label.add "labE0" [cVarDef "a"];));
  !! ignore (Trace.step_backtrack_on_failure (fun () ->
       Label.add "labE1" [cVarDef "a"];));
  !! ignore (Trace.step_backtrack_on_failure (fun () ->
       Label.add "labE2" [cVarDef "a"];
       failwith "trigger backtrack"));
  !! Trace.failure_expected (fun _e -> true) (fun () ->
       Label.add "labE3" [cVarDef "a"];
       failwith "trigger expected failure");
  (* Uncomment to see a partial trace
  !! Trace.failure_expected (fun _e -> true) (fun () ->
       Label.add "labE4" [cVarDef "a"]);*)

)
