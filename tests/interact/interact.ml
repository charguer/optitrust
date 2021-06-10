(* Usage:
      make optitrust && make interact.out
   or
      F6 on a given line
*)

open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  let show = Generic.target_show in
  !! Label.add "m0" [cVarDef "a"];
  (* The show command are ignored in batch mode,
     and the execute properly in interactive mode,
     showing only the result of one show command at a time. *)
  show [cVarDef "a"];
  show [cVarDef "b"];
  Generic.target_show [cVarDef "a"];

  (* Showing operation with step at front *)
  !! Label.add "m1" [cVarDef "b"];
  (* Showing operation with reparse *)
  !!! Label.add "m2" [cVarDef "a"];
  (* Showing two operations at once *)
  !!! Label.add "m3" [cVarDef "a"];
      Label.add "m4" [cVarDef "a"];

  (* Trace.dump() is called implicitly called at the end of file;
     this function handles the case where the cursor was after the last '!!'. *)
)

