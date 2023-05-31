open Optitrust
open Target

(* Usage:
       ./tester tests/interact/interact_traceview.ml
      cat tests/interact/interact_traceview_trace.txt
      chromium-browser interact_trace_trace.html
*)

(* This test can be used to test the generation of the javsacript trace [%_trace.js],
   used to display [%_trace.html].  *)

(* Dump a javascript file with the full trace at the end,
   when executing 'make interact_traceview.out'  *)
let  _ =
  Flags.dump_trace := true

let _ = Run.script_cpp (fun _ ->

    bigstep "first part";
    !! Label.add "lab1" [cVarDef "a"];
    !! Label.add "lab2" [cVarDef "a"];
    bigstep "second part";
    !! Label.add "lab3" [cVarDef "a"];
    !! Label.add "lab4" [cVarDef "a"];
    !! Label.add "lab5" [cVarDef "a"];
    bigstep "third part";
    !! Label.add "lab6" [cVarDef "a"];
    !! Label.add "lab7" [cVarDef "a"];

)
