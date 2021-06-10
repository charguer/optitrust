open Ast
open Target

let add (label : string) : Target.Transfo.t =
  Target.apply_on_target (Label_core.add label)

let remove : Target.Transfo.t =
  Target.apply_on_target (Label_core.remove)


let remove_multiple (tgs : target list) =
  List.fold_left (fun () x-> remove x )() tgs


(* TODO: Remove these later after fixing all other Generic which depend on this one *)

(* delete the label with the given prefix *)
let rec delete_label (label : string) (t : trm) : trm =
  match t.desc with
  | Trm_labelled (l, t') when l = label -> t'
  | _ -> trm_map (delete_label label) t

(* delete the labels which have a prefix in the list *)
let delete_labels (sl : string list) (t : trm) : trm =
  let rec aux (s : string) (t : trm) : trm =
    match t.desc with
    | Trm_labelled (l, t')
         when Str.string_match (Str.regexp (Str.quote s)) l 0 ->
       t'
    | _ -> trm_map (aux s) t
  in
  List.fold_left (fun t l -> aux l t) t sl

