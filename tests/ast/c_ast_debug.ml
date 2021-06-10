
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
(* TODO: move all to target_one.ml *)
  let show = Generic.target_show in
  show [ cTypDef "vect"];
  show [ cTypDef "myvect"];

  show [ cMulti; cVar "x"];
  show [ cMulti; cVar ~substr:true "x"];
  show [ cMulti; cVar ~regex:true "[xy]"];
  show [ cMulti; cVar ~regex:true ~substr:true "[xy]"];
)
