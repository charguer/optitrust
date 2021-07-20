open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Loop.hoist "x_step" [cVarDef "x"]; (* => rename to hoist_without_detach *)
  !! Loop.hoist "z_step" [cVarDef "z"]; (* => rename to hoist_without_detach *)
  (* !! Loop.hoist "y_step" [cFor "j"]; => move to combi level *)
) (* TODO: no generation of the update to step *)

(*
  Loop.hoist_without_detach   as core
  Loop.hoist = detach if needed + Loop.hoist_without_detach
*)

(*


if before
  int x = 2
first need to detach it.
   int x
   x = 2
by detaching first of all.
=> but this could happen at the "combi" level
*)