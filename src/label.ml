open Ast
open Target
open Tools

let label_add (tg : target) (label : string) : unit =
  apply_to_targets tg (fun p t ->
    Label_core.label_add p label t)

let label_rem (tg : target) : unit =
  apply_to_targets tg (fun p t ->
    Label_core.label_rem p t)


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


(* label the given path with the *)
let add_label (label : string) (tr : target) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "add_label: no matching subterm\n";
     t
  | [dl] -> apply_local_transformation (trm_labelled label) t dl
  | _ ->
     (*
         folding works since no path in epl is the prefix of a subsequent path
      *)
     foldi
       (fun i -> apply_local_transformation
                   (trm_labelled (label ^ "_" ^ string_of_int i)))
       t epl
