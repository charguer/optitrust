open Ast

(* [add m ] adds marks m to node t 
    if m = "" then do nothing *)
let add (m : mark) (tg : Target.target) : unit = 
  if m = "" then () else Target.apply_on_targets (Marks_core.add m) tg

(* [add_between m ] adds mark m at he location of the relative target [tg] *)
let add_between (m : mark) (tg : Target.target) : unit =
  if m = "" then () 
    else
      Target.apply_on_targets_between( 
        fun t (p,i) -> Target.apply_on_path (fun t -> trm_add_mark_between i m t) t p) tg

(* [remove m] removes mark m from node t *)
let remove (m : mark) : Target.Transfo.t =
  Target.apply_on_targets (Marks_core.remove m)

(* [clean] cleans all marks from node t *)
let clean : Target.Transfo.t = 
  Target.apply_on_targets (Marks_core.clean)