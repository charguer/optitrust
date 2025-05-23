open Prelude

(* DEPRECATED
(** [transform_on f_get f_set t]: applies f_get or f_set depending on the fact if
    [t] is a get operation or a set operation,
      [f_get] - the get operation that is going to be applied,
      [f_set] - the set operation that is going to be applied,
      [t] - the ast of the node where the operation is applied to. *)
let transform_on (f_get : trm -> trm) (f_set : trm -> trm) (t : trm) : trm =
  let error = "Accesses_core.transform_on: expected either a get or a set operation" in
  let (f,args) = trm_inv ~error trm_apps_inv t in
  if is_get_operation t
    then f_get t
    else if is_set_operation t
     then begin match args with
      | [addr; targ] ->
        trm_replace (Trm_apps (f, [addr; f_set targ], [])) t
      | _ -> trm_fail t "Accesses_core.transform_on: expected either a get or a set operation"
      end
    else trm_fail t "Accesses_core.transform_on: expected a get operation"
*)

(** [intro_on t]: changes encodings "struct_get(get (t), f)" to "get(struct_access (t, f))",
      [t] - ast of the node where the accesses can be found. *)
let intro_on (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, [arg], _, _) ->
      begin match trm_prim_inv f with
      | Some (struct_typ, Prim_unop (Unop_struct_get x)) ->
        begin match arg.desc with
        | Trm_apps (_, [arg1], _, _) when is_get_operation arg ->
          trm_get ~annot:arg.annot (trm_apps (trm_unop struct_typ (Unop_struct_access x)) [arg1])
        | _ -> t
        end
      | _ -> trm_map aux t
      end
    | _ -> trm_map aux t
  in
  aux t
