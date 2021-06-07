open Optitrust
open Run
let _ = 
  run
    (
      fun _ -> 
      set_init_source "local_other_name.cpp";
      
      Sequence.sub 0 1 [cFunDef "main"] ;

      Generic.local_other_name "T" "a" "x" ();
      dump()
      
    )