open Optitrust
open Target

(* TODO: The outcome is not correct *)
let _ = Run.script_cpp (fun () ->
  !! Loop.hoist "x_step" [cFor "i"];
  !! Loop.hoist "y_step" [cFor "j"];
)


(*

LATER: when we have references in optitrust, we could generate:
    int& x = x_step[i];
    x = t[i];
    u[i] = x;

if before
  int x = 2
first need to detach it.
   int x
   x = 2
by detaching first of all.
*)