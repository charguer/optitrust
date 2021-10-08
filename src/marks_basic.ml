open Ast
(* [add m ] adds marks m to node t *)
let add (m : mark) : Target.Transfo.t =
  Target.apply_on_targets (Marks_core.add m)

(* [remove m] removes mark m from node t *)
let remove (m : mark) : Target.Transfo.t =
  Target.apply_on_targets (Marks_core.remove m)

(* [clean] cleans all marks from node t *)
let clean : Target.Transfo.t = 
  Target.apply_on_targets (Marks_core.clean)