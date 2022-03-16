open Optitrust
open Target

(* FOR DEBUG:
   make optitrust && rm -f interact_dumpbigstesps_*.* && make interact_dumpbigsteps_out.cpp && ls bigsteps
   gedit bigsteps/*.cpp &
*)

(* This test can be used to test the generation of big-steps output files. *)

let  _ =
  Flags.dump_big_steps := Some "bigsteps"

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
