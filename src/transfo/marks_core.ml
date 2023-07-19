open Syntax

(* [add_aux m t]: adds mark [m] to the node [t],
     [m] - the mark which is addded,
     [t] - the trm that is going to be marked. *)
let add_aux (m : mark) (t : trm) : trm =
  trm_add_mark m t

(* [add m t p]: applies [add_aux] at trm [t] with path [p]. *)
let add (m : mark) : Target.Transfo.local =
  Target.apply_on_path (add_aux m)

(* [add_between_aux index m t]: adds mark [m] at [index] in sequence [t],
     [index] - the relative index where the mark should be added,
     [m] - the mark that is addded,
     [t] - ast of the where the inbetween mark is going to be added. *)
let add_between_aux (index : int) (m : mark) (t : trm) : trm =
  trm_add_mark_between index m t

(* [add_between index m t p]: applies [add_between_aux] at trm [t] with index [p]. *)
let add_between (index : int) (m : mark) : Target.Transfo.local =
  Target.apply_on_path (add_between_aux index m)

(* [remove_aux m t]: removes mark [m] from node t
     [m] - the mark which is removed,
     [t] - the ast of the trm where the mark is removed. *)
let remove_aux (m : mark) (t : trm) : trm =
  trm_rem_mark m t

(* [remove m t p]: applies [remove_aux] at trm [t] with path [p]. *)
let remove (m : mark) : Target.Transfo.local =
  Target.apply_on_path (remove_aux m)

(* [clean_aux m t]: removes all marks from [t],
     [t] - trm whose marks are going to be removed.*)
let clean_aux (t : trm) : trm =
  trm_remove_marks t

(* [clean t p]: applies [clean_aux] at trm [t] with path [p]. *)
let clean : Target.Transfo.local =
  Target.apply_on_path (clean_aux)
