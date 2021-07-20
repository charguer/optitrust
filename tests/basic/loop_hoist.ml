open Optitrust
open Target

(* TODO: The outcome is not correct *)
let _ = Run.script_cpp (fun () ->
  !! Loop.hoist "x_step" [cVarDef "y"]; (* => rename to hoist_without_detach *)
  (* !! Loop.hoist "y_step" [cFor "j"]; => move to combi level *)
)

(*
  Loop.hoist_without_detach   as core
  Loop.hoist = detach if needed + Loop.hoist_without_detach
*)

(*

TODO: we should generate directly this code:
    int& x = x_step[i];
    x = t[i];
    u[i] = x;




if before
  int x = 2
first need to detach it.
   int x
   x = 2
by detaching first of all.
=> but this could happen at the "combi" level
*)