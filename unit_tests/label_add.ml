(* for the moment, this ml file is in the filter-out list in the Makefile *)

(***
    - on the declaration for 'x', add label 'start'
    - on the for loop on 'i', add label 'loop'
    - on the conditional, add label 'cond'
    - on the increment operations, add label 'incr_{$i}'
      (the numbering is automatically handled by add_label)
    - on the return operation, add label 'stop'
*)

open Optitrust

let _ = 
  run
  ( fun _ ->
    set_init_source"label_add.cpp";
    add_label "start" [cVarDef "x"] ;
    add_label "loop" [cFor "i"];
    add_label "cond" [cIf ~then_:[cVar "x++"] ()] ;
    add_label "incr_1" [cIf (); cVar "x"];
    add_label "incr_2" [cIf ();cVar "i" ];    
    add_label "stop" [cInstrSubstr "return"];
    dump()
  )