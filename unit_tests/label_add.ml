(* for the moment, this ml file is in the filter-out list in the Makefile *)

(***
    - on the declaration for 'x', add label 'start'
    - on the for loop on 'i', add label 'loop'
    - on the conditional, add label 'cond'
    - on the increment operations, add label 'incr_{$i}'
      (the numbering is automatically handled by add_label)
    - on the return operation, add label 'stop'
*)

open ScriptTools

let _ = 
  run
  ( fun _ ->
    set_init_source"label_add.cpp";
    add_label "start" [cVarDef ~name:"x" ()] ;
    add_label "loop" [cFor ~init:[cVarDef ~name:"i" ()] ()];
    add_label "cond" [cIf ~then_:[cVar ~name:"x++" ()] ()] ;
    add_label "incr_1" [cIf (); cVar ~name:"x"()];
    add_label "incr_2" [cIf ();cVar ~name:"i" ()];    
    add_label "stop" [cInstrSubstr "return"];
    dump()
  )