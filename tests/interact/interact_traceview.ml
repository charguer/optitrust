open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    bigstep "first part";
    !! Label.add "lab1" [cVarDef "a"];
    !! Label.add "lab2" [cVarDef "a"];
    bigstep "second part";
    (* Uncomment the line below to see a partial trace
     !! if true then failwith "the error message";
    *)
    !! Label.add "lab3" [cVarDef "a"];
    !! Trace.msg "debug message before\nlab4 \n";
       Label.add "lab4" [cVarDef "a"];
       Trace.msg "debug message after\nlab4 \n";
    !! Label.add "lab5" [cVarDef "a"];
    bigstep "third part";
    !! Label.add "lab6" [cVarDef "a"];
    !! Label.add "lab7" [cVarDef "a"];

)
