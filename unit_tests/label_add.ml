(* for the moment, this ml file is in the filter-out list in the Makefile *)

(***
    - on the declaration for 'x', add label 'start'
    - on the for loop on 'i', add label 'loop'
    - on the conditional, add label 'cond'
    - on the increment operations, add label 'incr_{$i}'
      (the numbering is automatically handled by Label.add)
    - on the return operation, add label 'stop'
*)

open Optitrust

let _ = 
  run
  ( fun _ ->
    set_init_source"label_add.cpp";
    Label.add "start" [cVarDef "x"] ;
    Label.add "loop" [cFor "i"];
    Label.add "cond" [cIf ~then_:[cVar "x++"] ()] ;
    Label.add "incr_1" [cIf (); cVar "x"];
    Label.add "incr_2" [cIf ();cVar "i" ];    
    Label.add "stop" [cInstrSubstr "return"];
    dump()
  )