open Optitrust
open Run
let _ = run_unit_test 
    ( fun _ ->
        let show = Generic.target_show in
        let clean = Generic.clean_target_decorators in
        show [cMulti;cVar "x_step"];
        clean;
        show  [cVarDef "i"] ;
        clean;
        show  [cFor "i"];
        clean;
        show  [cIf ~then_:[cVar "x++"] ()] ;
        clean;
        show  [cIf ()] ;
        clean;
        show  [cIf (); cVar "x"];
        clean;
        show  [cIf ();cVar "i"];    
        clean;
        show  [cInt 3];    
        clean;
        show  [cInstr "return"];
    
        dump()
    )