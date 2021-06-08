
open Optitrust
open Run

let _ = run_unit_test (fun _ ->
(* TODO: move all to target_one.ml *)

  show_target [ cTypDef "vect"];
  show_target [ cTypDef "myvect"];

  show_target [ cMulti; cVar "x"];
  show_target [ cMulti; cVar ~substr:true "x"];
  show_target [ cMulti; cVar ~regex:true "[xy]"];
  show_target [ cMulti; cVar ~regex:true ~substr:true "[xy]"];
)
