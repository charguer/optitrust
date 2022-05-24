open Ast

(* [add m tg]: adds mark [m] to the trm that correpsonds to target [tg].
   NOTE: if m = "" then does nothing. *)
let add (m : mark) (tg : Target.target) : unit =
  if m = "" then () else Target.apply_on_targets (Marks_core.add m) tg

(* [add_between m]: adds mark [m] at the location of the relative target [tg].
   NOTE: if m = "" then does nothing. *)
let add_between (m : mark) (tg : Target.target) : unit =
  if m = "" then ()
    else
      Target.apply_on_targets_between(
        fun t (p,i) ->
         Target.apply_on_path (fun t -> trm_add_mark_between i m t) t p) tg

(* [remove m tg]: removes mark m from the trm that corresponds to target [tg]. *)
let remove (m : mark) : Target.Transfo.t =
  Target.apply_on_targets (Marks_core.remove m)

(* [clean tg]: cleans all the marks from the trm that corresponds to target [tg]. *)
let clean : Target.Transfo.t =
  Target.apply_on_targets (Marks_core.clean)
