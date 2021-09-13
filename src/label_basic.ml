open Target

(* [add label tg] adds a C-label named [label] to the front of the
   terms matching the target [tg]. *)
let add (label : string) : Target.Transfo.t =
  Target.apply_on_targets (Label_core.add label)

(* [remove label tg] removes a C-label named [label] matched by th target [tg]. *)
let remove : Target.Transfo.t =
  Target.apply_on_targets (Label_core.remove)

(* [remove_multiple tgs] removes a list  of C-labels*)
let remove_multiple (tgs : target list) =
  List.fold_left (fun () x-> remove x )() tgs
