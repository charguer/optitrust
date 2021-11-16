open Ast

(* [add m ] adds marks m to node t *)
let add (m : mark) : Target.Transfo.t =
  Target.apply_on_targets (Marks_core.add m)

(* [add_between m ] adds mark m at he location of the relative target [tg] *)
let add_between (m : mark) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq )
    (fun (p,i) t -> Marks_core.add_between i m t p )

(* [remove m] removes mark m from node t *)
let remove (m : mark) : Target.Transfo.t =
  Target.apply_on_targets (Marks_core.remove m)

(* [clean] cleans all marks from node t *)
let clean : Target.Transfo.t = 
  Target.apply_on_targets (Marks_core.clean)