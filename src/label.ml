open Target

let add (label : string) : Target.Transfo.t =
  Target.apply_on_target (Label_core.add label)

let remove : Target.Transfo.t =
  Target.apply_on_target (Label_core.remove)


let remove_multiple (tgs : target list) =
  List.fold_left (fun () x-> remove x )() tgs

