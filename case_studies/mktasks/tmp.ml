open Optitrust
open Target
open Ast
open Apac_core

(*
  Get dep_kind of function argument
  todo : handle typedef and using aliasing 
*)

let is_base_type (t : typ) : bool =
  match t.typ_desc with
  | Typ_int | Typ_float | Typ_double | Typ_bool | Typ_char | Typ_string | Typ_unit -> true
  (* class, struct, union ... *)
  (* also typedef and using ! which are not wanted *)
  | Typ_constr _ -> true
  | _ -> false

let rec is_dep_in_aux (t : typ) : bool =
  match t.typ_desc with
  (* base type const *)
  | Typ_const t when is_base_type t -> true
  (* ptr const *)
  | Typ_const { typ_desc = Typ_ptr { ptr_kind = Ptr_kind_mut ; inner_typ = t } } -> is_dep_in_aux t
  | Typ_const { typ_desc = Typ_array (t, _); _ } -> is_dep_in_aux t
  (* ptr *)
  | Typ_ptr {ptr_kind = Ptr_kind_mut; _ } -> false
  | Typ_array (t, _) -> false
  (* base type *)
  | _  when is_base_type t -> false
  (* should not encounter *)
  | Typ_ptr {ptr_kind = Ptr_kind_ref; _ } -> assert false
  | Typ_const _ -> assert false
  | _ -> assert false

(* must replace typedef and using aliasing by their definition before *)
(* does not handle auto *)
let is_dep_in (t : typ) : bool =
  match t.typ_desc with
  (* reference *)
  | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = t } -> 
    begin match t.typ_desc with
    (* void & *)
    | Typ_unit -> assert false
    (* cons void & *)
    | Typ_const { typ_desc = Typ_unit } -> assert false

    | _ -> is_dep_in_aux t
    end
  (* void *)
  | Typ_unit -> assert false
  (* base type *)
  | _ when is_base_type t -> true

  | _ -> is_dep_in_aux t

let dep_kind_of_typ (t : typ) : dep_kind =
  if is_dep_in t then Dep_kind_in else Dep_kind_inout


(*
  constify argument
*)

let rec constify_arg_aux (ty : typ) : typ =
  let annot = ty.typ_annot in
  let typ_attributes = ty.typ_attributes in
  match ty.typ_desc with 
  | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty} ->
    typ_const (typ_ptr Ptr_kind_mut (constify_arg_aux ty))
  | Typ_const {typ_desc = Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty }; typ_annot = t_an; typ_attributes = t_at} ->
    typ_const ~annot ~typ_attributes (typ_ptr ~annot:t_an ~typ_attributes:t_at Ptr_kind_mut (constify_arg_aux ty))
  | Typ_const _ -> ty
  | _ -> typ_const ty

let constify_arg (ty : typ) : typ =
  let annot = ty.typ_annot in
  let typ_attributes = ty.typ_attributes in
  match ty.typ_desc with 
  | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } -> 
    typ_ptr ~annot ~typ_attributes Ptr_kind_ref  (constify_arg_aux ty)
  | _ -> constify_arg_aux ty