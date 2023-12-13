open Optitrust
open Target

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
    (* Examples of show functions *)
    !! Show.ast ();
    !! Show.res ();
    !! Show.target [cVarDef ""];
    bigstep "third part";
    (* Try task "View diff using internal syntax" *)
    !! Label.add "lab6" [cVarDef "a"];
    !! Label.add "lab7" [cVarDef "a"];
)
