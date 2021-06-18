open Optitrust
open Target
let _ = Run.script_cpp 
    ( fun _ ->
        show [cVarDef "t"];
        !!show [cMulti;cVar "x_step"];
        show  [cVarDef "i"] ;
        show  [cFor "i"];
        show  [cIf ~then_:[cVar "x++"] ()] ;
        show  [cIf ()] ;
        show  [cIf (); cVar "x"];
        show  [cIf ();cVar "i"];    
        show  [cInt 3];    
        show  [cInstr "return"];
    )