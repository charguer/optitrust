open Prelude

(** [typid_to_typedef tid ]: gets the declaration of a typedef with id [tid]. *)
let typid_to_typedef (tid : typconstrid) : typedef option =
  let rec aux (t : trm) : typedef option =
    match t.desc with
    | Trm_seq tl -> Mlist.find_map aux tl
    | Trm_namespace (_, t, _) -> aux t
    | Trm_typedef ({typdef_typid = tid1;_} as td)
      when tid = tid1 -> Some td
    | _ -> None
  in

  let t_root = Target.get_ast () in
  match t_root.desc with
  | Trm_seq _ -> aux t_root
  | _ -> trm_fail t_root "Context.typid_to_typedef: expected the ast of the main file"

(** [record_typ_to_typid ty]: gets the id of the record type [ty]. *)
let record_typ_to_typid (ty : typ) : typconstrid option =
  let rec aux (ty : typ) : typconstrid option =
    match ty.typ_desc with
    | Typ_const ty -> aux ty
    | Typ_ptr { inner_typ = ty; _ } -> aux ty
    | Typ_constr (_, tid, _) ->
      begin match typid_to_typedef tid with
      | Some (td) ->
        begin match td.typdef_body with
        | Typdef_alias ty -> aux ty
        | Typdef_record _ -> Some (tid)
        | _ -> None
        end
      | None -> assert false
      end
    | _ -> None
  in
  aux ty

(** [typid_to_trm tid ]: gets the trm of a typedef with id [tid]. *)
let typid_to_trm (tid : typconstrid) : trm option =
  let rec aux (t : trm) : trm option =
    match t.desc with
    | Trm_seq tl -> Mlist.find_map aux tl
    | Trm_namespace (_, t, _) -> aux t
    | Trm_typedef ({typdef_typid = tid1;_})
      when tid = tid1 -> Some t
    | _ -> None
  in

  let t_root = Target.get_ast () in
  match t_root.desc with
  | Trm_seq _ -> aux t_root
  | _ -> trm_fail t_root "Context.typid_to_typedef: expected the ast of the main file"
