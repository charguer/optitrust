open Optitrust
open Target

(* Dump a javascript file with the full trace at the end,
   when executing 'make interact_trace.out'  *)
let  _ = Flags.dump_trace := true

let _ = Run.script_cpp (fun _ ->
    !^ Label.add "lab1" [cVarDef "a"];
    !! Label.add "lab2" [cVarDef "a"];
    !^ Label.add "lab3" [cVarDef "a"];
    !! Label.add "lab4" [cVarDef "a"];
    !! Label.add "lab5" [cVarDef "a"];
)
