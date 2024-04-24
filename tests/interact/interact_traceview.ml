open Optitrust
open Prelude

(** This unit test is for testing the trace generation *)

let _ = Flags.check_validity := true

(* Use the line below to see all details in the full trace,
   and not only when requesting a per-step trace
let _ = Flags.detailed_trace := true
*)

(* Use the line below to shrink ASTs during diffs. *)
let _ = Flags.use_light_diff := true

(* Use the line below to control use of clang-format *)
let _ = Flags.use_clang_format := false

(* Use the line below to control reporting of execution time *)
let _ = Flags.report_exectime := true

(* Use the line below to see encoded syntax in trace
let _ = Flags.print_optitrust_syntax := true
*)

(* Use the line below to see on stdout the opening and closing of steps:
let _ = Trace.debug_open_close_step := true
*)

(* LATER: test a shortcut triggering only_big_steps to view the trace
    associated with a big-step. Or perhaps attempt to automatically
    trigger this flag if the user is on a line calling "bigstep" *)

let _ = Run.script_cpp (fun _ ->

  (* Try task "view trace" on this program; debug messages below should appead in the trace *)
  bigstep "first part";
  !! Label.add "lab1" [cVarDef "a"];
  !! Label.add "lab2" [cVarDef "a"];

  (* Uncomment next line to see an error
  !! Label.add "lab2" [cVarDef "xx"];
  *)

  (* Details of multi-target processing *)
  bigstep "second part";
  !! Target.iteri (fun i p ->
       Label.add ("occ" ^ string_of_int i) [cPath p])
     [nbMulti; cVarDef ""];

  bigstep "third part";
  (* Uncomment the line below to see a partial trace
    !! if true then failwith "the error message";
  *)
  !! Label.add "lab3" [cVarDef "a"];
  (* Try task "View diff" on the line below; debug messages below should appear on stdout *)
  !! Trace.msg "debug message before\nlab4 \n";
      Label.add "lab4" [cVarDef "a"];
      Trace.msg "debug message after\nlab4 \n";
  (* Example of a reparse step *)
  !! Trace.reparse();
  !! Label.add "lab5" [cVarDef "a"];
  (* Examples of show functions with output in browser *)
  !! Show.ast ();
  !! Show.ast ~internal:false ();
  !! Resources.ensure_computed ();
  !! Show.res ();
  !! Show.ctx ();
  !! Show.delta ();
  !! Show.target [cVarDef ""];
  (* Example with two show operations within the same small-step;
    the diff would show empty, but we get a warning.
  !! Show.ast (); Show.ast ();
  *)
  (* Examples of show functions with output on stdout *)
  (* !! ShowAt.trm []; *)
  bigstep "fourth part";
  (* Try task "View diff using internal syntax" *)
  !! Label.add "lab6" [cVarDef "a"];
  (* Demo of substeps modifying the ast directly
     (only transformations should do this) *)
  let ast6 = Trace.ast() in
  !! Label.add "lab7" [cVarDef "a"];
  !! Trace.set_ast ast6;
     Label.add "lab8" [cVarDef "a"];
     Trace.set_ast ast6;
  (* Example of trustme section *)
  !! Trace.trustme "I know what I'm doing" (fun () ->
     Label.add "labX1" [cVarDef "a"];
     Trace.set_ast ast6;
     Label.add "labX2" [cVarDef "a"];
  );
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
