open Optitrust
open Run

(* Works *)

let _ = run_unit_test ( fun _ ->
    Label.add "start" [cVarDef "x"] ;
    Label.add "loop" [cFor "i"];
    Label.add "cond" [cIf ~then_:[cVar "x"] ()] ;
    Label.add "incr_1" [cIf (); cVar "x"];
    Label.add "incr_2" [cIf ();cVar "i" ];    
    Label.add "stop" [cInstr "return"];
  )