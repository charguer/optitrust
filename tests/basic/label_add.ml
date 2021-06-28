open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
    !! Label.add "start" [cVarDef "x"] ;
    !! Label.add "loop" [cForSimple "i"];
    !! Label.add "cond" [cIf ()];
    !! Label.add "incr_1" [cIf (); sInstr "x++"];
    !! Label.add "incr_2" [cIf (); sInstr "i++" ];    
    !! Label.add "stop" [cReturn];
)
