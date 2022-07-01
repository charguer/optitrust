open Optitrust
open Target
open Ast
open Apac_core

(*
  Get dep_kind of function argument
  todo : handle typedef and using aliasing 
*)

let is_typdef_alias (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_constr (_, id, _) ->
    begin match Context.typid_to_typedef id with
    | Some (td) -> 
      begin match td.typdef_body with 
      | Typdef_alias _ -> true
      | _  -> false
      end
    | None -> false 
    end
  | _ -> false

let get_inner_typedef_alias (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_constr (_, id, _) ->
    begin match Context.typid_to_typedef id with
    | Some (td) -> 
      begin match td.typdef_body with 
      | Typdef_alias ty -> Some (ty) 
      | _  -> None
      end
    | None -> None 
    end
  | _ -> None
  
let rec is_base_type (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_int | Typ_float | Typ_double | Typ_bool | Typ_char | Typ_string | Typ_unit -> true
  | Typ_constr _ -> 
    begin match get_inner_typedef_alias ty with
    (* alias *)
    | Some (ty) -> is_base_type ty
    (* class, struct, union, enum ... *)
    | None -> true
    end
  | _ -> false

let rec is_dep_in_aux (ty : typ) : bool =
  match ty.typ_desc with
  (* unwrap alias *)
  | Typ_constr _ | Typ_const _ when is_typdef_alias (get_inner_const_type ty) -> 
    begin match get_inner_typedef_alias (get_inner_const_type ty) with
    | Some (ty) -> is_dep_in_aux ty
    | None -> assert false
    end
  (* base type const *)
  | Typ_const ty when is_base_type ty -> true
  (* ptr const *)
  | Typ_const { typ_desc = Typ_ptr { ptr_kind = Ptr_kind_mut ; inner_typ = ty } } -> is_dep_in_aux ty
  | Typ_const { typ_desc = Typ_array (ty, _); _ } -> is_dep_in_aux ty
  (* ptr *)
  | Typ_ptr {ptr_kind = Ptr_kind_mut; _ } -> false
  | Typ_array (ty, _) -> false
  (* base type *)
  | _  when is_base_type ty -> false
  (* should not encounter *)
  | Typ_ptr {ptr_kind = Ptr_kind_ref; _ } -> assert false
  | Typ_const _ -> assert false
  | _ -> assert false

(* does not handle auto *)
let rec is_dep_in (ty : typ) : bool =
  match ty.typ_desc with
  (* unwrap alias *)
  | Typ_constr _ | Typ_const _ when is_typdef_alias (get_inner_const_type ty) -> 
    begin match get_inner_typedef_alias (get_inner_const_type ty) with
    | Some (ty) -> is_dep_in ty
    | None -> assert false
    end
  (* reference *)
  | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } -> 
    begin match ty.typ_desc with
    (* void & *)
    | Typ_unit -> fail None "is_dep_in: void & as argument"
    (* const void & *)
    | Typ_const { typ_desc = Typ_unit } -> fail None "is_dep_in: const void & as argument"

    | _ -> is_dep_in_aux ty
    end
  (* const void *)
  | Typ_const { typ_desc = Typ_unit } -> fail None "is_dep_in: const void as argument"
  (* base type *)
  | _ when is_base_type ty -> true

  | _ -> is_dep_in_aux ty

let dep_kind_of_typ (ty : typ) : dep_kind =
  if is_dep_in ty then Dep_kind_in else Dep_kind_inout


(*
  constify argument
*)

let rec constify_arg_aux (ty : typ) : typ =
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  match ty.typ_desc with 
  | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty} ->
    typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (constify_arg_aux ty))
  | Typ_const {typ_desc = Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty }; typ_annot = t_an; typ_attributes = t_at} ->
    typ_const (typ_ptr ~annot:t_an ~attributes:t_at Ptr_kind_mut (constify_arg_aux ty))
  | Typ_const _ -> ty
  | _ -> typ_const ty

let constify_arg (ty : typ) : typ =
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  match ty.typ_desc with 
  | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } -> 
    typ_ptr ~annot ~attributes Ptr_kind_ref  (constify_arg_aux ty)
  | _ -> constify_arg_aux ty