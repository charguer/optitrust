open Optitrust
open Target

(* FOR DEBUG:
   make optitrust && rm -f interact_trace_trace.* && make interact_trace_trace.html
*)

(* This test can be used to test the generation of the javsacript trace [%_trance.js],
   used to display [%_trace.html]. It can also be used to test the [alt+f6] shortcut
   for viewing big steps interactively. *)

(* Dump a javascript file with the full trace at the end,
   when executing 'make interact_traceview.out'  *)
let  _ = Flags.dump_trace := true

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

    !^ Label.add "info1" [cVarDef "b"];
    !! Label.add "info2" [cVarDef "b"];
    !^ Label.add "info3" [cVarDef "b"];
    !! Label.add "info4" [cVarDef "b"];
    !! Label.add "info5" [cVarDef "b"];
    !^ Label.add "info6" [cVarDef "b"];
    !! Label.add "info7" [cVarDef "b"];

)