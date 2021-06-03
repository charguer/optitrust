(* for the moment, this ml file is in the filter-out list in the Makefile *)

(*

   TODO: the current implementation of delete_labels says:
      "delete the labels which have a prefix in the list"
   and the implementation of delete_label is:
      "delete the label which match exactly the label".

   It would be nice to make the implementations more general and more uniform:

   - delete_label could take a string interpreted as a regular expression
     if an optional argument ?regexp:bool  is set to true.
   - delete_labels simply iterates delete_label

   Example:
      delete_label ~regexp:true "incr_.*"   should delete both incr_ labels


   delete_labels ~regexp:true ["incr_.*"; "loop"; "stop"]

*)

open Optitrust

let _ = 
   run
   ( fun _ -> 
      set_init_source"label_rem.cpp";
      delete_labels ["loop"; "cond";"incr_1";"incr_2";"stop"];
      Label.add "condincr" [cIf ~then_:[cVar ~name:"x++" ()] ()] ;
      dump()

   )