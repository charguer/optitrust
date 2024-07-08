open Ast
open Contextualized_error

let typ_annot_default = { trm_annot_default with trm_annot_cstyle = [Type] }

let typ_make ?loc desc = trm_make ~annot:typ_annot_default ?loc desc
let typ_alter ?loc ?desc t = trm_alter ?loc ?desc t
let typ_replace desc t = trm_replace desc t

(***************************** Typ vars *************************** *)

let typ_namespace = ["__typ"]

let toplevel_typvar ?(namespaces=[]) (name: string) : typvar = toplevel_var ~namespaces:(typ_namespace @ namespaces) name
let name_to_typvar ?(namespaces=[]) (name: string): typvar =
  if name = "" then failwith "trying to create a type without name" else
  name_to_var ~namespaces:(typ_namespace @ namespaces) name

(** [remove_typ_namespace var]: remove the type namespace from the variable [var] for printing in a type context. *)
let remove_typ_namespace (var: typvar): var =
  let stripped_nss = match var.namespaces with
    | ns :: nss when ns = List.hd typ_namespace -> nss
    | _ -> failwith "variable %s is not in the type namespace" (var_to_string var)
  in
  { var with namespaces = stripped_nss }

let typ_var (x: typvar): typ = typ_make (Trm_var x)

(* We may soon need a type for types:
let typ_type_var = toplevel_typvar "Type"
let typ_type = typ_var typ_type_var
*)

let typ_unit_var = toplevel_typvar "unit"
let typ_unit = typ_var typ_unit_var

let typ_apps ?loc (t: typ) (args: trm list) =
  typ_make ?loc (Trm_apps (t, args, []))

let typ_auto_var = toplevel_typvar "auto"
let typ_auto = typ_var typ_auto_var

(* Types int (resp. uint) represent signed (resp. unsigned) integers without bound checks *)
let typ_int_var = toplevel_typvar "int"
let typ_int = typ_var typ_int_var
let typ_uint_var = toplevel_typvar "uint"
let typ_uint = typ_var typ_uint_var

(* Unsigned and signed version of types with pointer size *)
let typ_usize_var = toplevel_typvar "usize"
let typ_usize = typ_var typ_usize_var
let typ_isize_var = toplevel_typvar "isize"
let typ_isize = typ_var typ_isize_var

(* Types for floating point values with 32 bits (C float's) or 64 bits (C double's) *)
let typ_f32_var = toplevel_typvar "f32"
let typ_f32 = typ_var typ_f32_var
let typ_f64_var = toplevel_typvar "f64"
let typ_f64 = typ_var typ_f64_var

(* Type of booleans *)
let typ_bool_var = toplevel_typvar "bool"
let typ_bool = typ_var typ_bool_var

(* Fixed size integers *)
let typ_i8_var = toplevel_typvar "i8"
let typ_i8 = typ_var typ_i8_var
let typ_u8_var = toplevel_typvar "u8"
let typ_u8 = typ_var typ_u8_var

let typ_i16_var = toplevel_typvar "i16"
let typ_i16 = typ_var typ_i16_var
let typ_u16_var = toplevel_typvar "u16"
let typ_u16 = typ_var typ_u16_var

let typ_i32_var = toplevel_typvar "i32"
let typ_i32 = typ_var typ_i32_var
let typ_u32_var = toplevel_typvar "u32"
let typ_u32 = typ_var typ_u32_var

let typ_i64_var = toplevel_typvar "i64"
let typ_i64 = typ_var typ_i64_var
let typ_u64_var = toplevel_typvar "u64"
let typ_u64 = typ_var typ_u64_var

let typ_const_var = toplevel_typvar "const"
let typ_const_constr = typ_var typ_const_var
(** [typ_const ?loc t]: wrapper for making the corresponding const type.
  After decoding, const types should only appear behind a pointer.
  LATER: Think about constness of struct members, for now we only support structs without const fields. *)
let typ_const ?loc (t : typ) : typ =
  typ_apps ?loc typ_const_constr [t]

let typ_ptr_var = toplevel_typvar "ptr"
let typ_ptr_constr = typ_var typ_ptr_var
(** [typ_ptr ?loc t]: build a pointer type around t *)
let typ_ptr ?loc (t : typ) : typ = typ_apps ?loc typ_ptr_constr [t]

(* Type of characters and strings *)
let typ_char_var = toplevel_typvar "char"
let typ_char = typ_var typ_char_var
let typ_string = typ_ptr (typ_const typ_char)

let typ_ref_var = toplevel_typvar "reference"
let typ_ref_constr = typ_var typ_ref_var
(** [typ_ref ?loc t]: build a C++ reference type around t (these should never exist after decoding) *)
let typ_ref ?loc (t: typ): typ = typ_apps ?loc typ_ref_constr [t]

let typ_array_var = toplevel_typvar "array"
let typ_array_constr = typ_var typ_array_var
(** [typ_array ?loc ?size t]: array type constructor *)
let typ_array ?loc ?(size : trm option) (t : typ) : typ =
  typ_apps typ_array_constr ([t] @ (Option.to_list size))

let typ_tuple_var = toplevel_typvar "tuple"
let typ_tuple_constr = typ_var typ_tuple_var
(** [typ_tuple ?loc [t1; ...; tn]]: build the tuple type [t1 * ... * tn] *)
let typ_tuple ?loc (ts: typ list): typ =
  typ_apps ?loc typ_tuple_constr ts

let typ_fun_var = toplevel_typvar "fun"
let typ_fun_constr = typ_var typ_fun_var
(** [typ_fun ?loc args res]: function type constructor *)
let typ_fun ?loc (args : typ list) (res : typ) : typ =
  typ_apps ?loc typ_fun_constr (res :: args)

(** [typ_arbitrary s]: arbitrary string as type, need reparse to eliminate *)
let typ_arbitrary ?loc (s : string) : typ =
  typ_make ?loc (Trm_arbitrary (Typ s))

let typ_typeof_var = toplevel_typvar "typeof"
let typ_typeof_constr = typ_var typ_typeof_var
(** [typ_typeof ?loc expr]: type of the given expression, corresponds to the C++ type written decltype(expr) *)
let typ_typeof ?loc (expr : trm) =
  typ_apps typ_typeof_constr [expr]

(** [typ_add_attribute attr ty]: adds the attribute [attr] to the type [ty] *)
let typ_add_attribute (attr : attribute) (ty : typ) : typ =
  {ty with annot = { ty.annot with trm_annot_attributes = attr :: ty.annot.trm_annot_attributes } }

(** [typ_align align ty]: adds the alignas attribute to type ty *)
let typ_align (align : trm) (ty : typ) =
  typ_add_attribute (Alignas align) ty

(*****************************************************************************)

(** [typ_var_inv ty]: if the typ is a variable, return this variable *)
let typ_var_inv (ty : typ) : typvar option =
  match ty.desc with
  | Trm_var v -> Some v
  | _ -> None

let typ_apps_inv (ty : typ) : (typvar * typ list) option =
  match ty.desc with
  | Trm_apps ({ desc = Trm_var v }, args, []) -> Some (v, args)
  | _ -> None

(** [typ_ptr_inv ty]: get the inner type of a const *)
let typ_const_inv (ty : typ) : typ option =
  match typ_apps_inv ty with
  | Some (v, [ty]) when var_eq v typ_const_var -> Some ty
  | _ -> None

(** [typ_ptr_inv ty]: get the inner type of a pointer *)
let typ_ptr_inv (ty : typ) : typ option =
  match typ_apps_inv ty with
  | Some (v, [ty]) when var_eq v typ_ptr_var -> Some ty
  | _ -> None

(** [typ_ref_inv ty]: get the inner type of a reference *)
let typ_ref_inv (ty : typ) : typ option =
  match typ_apps_inv ty with
  | Some (v, [ty]) when var_eq v typ_ref_var -> Some ty
  | _ -> None

let typ_array_inv (ty : typ) : (typ * trm option) option =
  match typ_apps_inv ty with
  | Some (v, [ty]) when var_eq v typ_array_var -> Some (ty, None)
  | Some (v, [ty; size]) when var_eq v typ_array_var -> Some (ty, Some size)
  | _ -> None

let rec typ_nested_array_inv (ty: typ) : typ * trm option list =
  match typ_array_inv ty with
  | Some (ty, sz) ->
    let ty, szs = typ_nested_array_inv ty in
    ty, sz :: szs
  | None -> ty, []

let typ_fun_inv (ty: typ) : (typ list * typ) option =
  match typ_apps_inv ty with
  | Some (v, res :: args) when var_eq v typ_fun_var -> Some (args, res)
  | _ -> None

let typ_constr_inv (ty: typ): var option =
  match typ_var_inv ty with
  | Some tv -> Some tv
  | None ->
    match typ_apps_inv ty with
    | Some (tv, _) -> Some tv
    | None -> None

(*****************************************************************************)

let typ_inv ?(error : string = "") (trm : trm) (k : typ -> 'a option) (t : typ) : 'a =
  match k t with
  | None -> if error = "" then assert false else trm_fail trm error
  | Some r -> r

(** [get_inner_ptr_type ty]: gets the underlying type of [ty] when [ty] is a generated pointer type *)
let get_inner_ptr_type (ty : typ) : typ =
  match typ_ptr_inv ty with
  | Some ty -> ty
  | None -> ty

(** [get_inner_array_type ty]: returns the underlying type of [ty] when [ty] is an array type. *)
let get_inner_array_type (ty : typ) : typ =
  match typ_array_inv ty with
  | Some (ty, _) -> ty
  | None -> ty

(** [get_inner_const_type ty]: gets the underlying type of [ty] when [ty] is a const type *)
let get_inner_const_type (ty : typ) : typ =
  match typ_const_inv ty with
  | Some ty -> ty
  | None -> ty

(** [get_inner_type ty]: returns the inner type of [ty] when [ty] is a pointer type, const type or an array type. *)
let get_inner_type (ty : typ) : typ =
  match typ_const_inv ty with
  | Some ty -> ty
  | None ->
    match typ_ptr_inv ty with
    | Some ty -> ty
    | None ->
      match typ_array_inv ty with
      | Some (ty, _) -> ty
      | None -> ty

(** [is_reference]: checks if the type is a reference type or not.
  FIXME: This function should NEVER be called outside decoding.
  C++ references MUST be eliminated during decoding phase ! *)
let is_reference (ty : typ) : bool =
  let ty = get_inner_ptr_type ty in
  match typ_ref_inv ty with
  | Some _ -> true
  | None -> false

(** [is_typ_const ty]: checks if [ty] is a const type *)
let is_typ_const (ty : typ) : bool =
  match typ_const_inv ty with
  | Some _ -> true
  | None -> false

(** [is_typ_unit t]: checks if the [t] has type void *)
let is_typ_unit (t : typ) : bool =
  match typ_var_inv t with
  | Some v when var_eq v typ_unit_var -> true
  | _ -> false

(** [is_typ_ptr ty]: checks if [ty] is a pointer type *)
let is_typ_ptr (ty : typ) : bool =
  match typ_ptr_inv ty with
  | Some _ -> true
  | _ -> false

(** [is_typ_ref ty]: checks if [ty] is a reference type *)
let is_typ_ref (ty : typ) : bool =
  match typ_ref_inv ty with
  | Some _ -> true
  | _ -> false

(** [is_typ_fun ty]: checks if [ty] is a function type *)
let is_typ_fun (ty : typ) : bool =
  match typ_fun_inv ty with
  | Some _ -> true
  | _ -> false

(** [is_typ_named name ty]: checks if [ty] is a type named [name] *)
let is_typ_named (name : var) (ty: typ) : bool =
  begin match typ_var_inv ty with
  | Some x -> var_eq x name
  | _ -> false
  end

(** [is_typ_array ty]: checks if [ty] is an array type. *)
let is_typ_array (ty : typ) : bool =
  match typ_array_inv ty with
  | Some _ -> true
  | _ -> false

(*****************************************************************************)

let typ_of_get (t : typ) : typ option =
  let t = get_inner_const_type t in
  Option.or_ (Option.map fst (typ_array_inv t)) (typ_ptr_inv t)

(** [typ_of_lit l]: get the type of a literal *)
let typ_of_lit (l : lit) : typ option =
  match l with
  | Lit_unit -> Some typ_unit
  | Lit_uninitialized -> None
  | Lit_bool _ -> Some typ_bool
  | Lit_int _ -> Some typ_int
  | Lit_float _ -> Some typ_f64
  | Lit_string _ -> Some typ_string
  | Lit_nullptr -> None

(*****************************************************************************)

(** [typedef_get_members ~access t]: returns all the memebers of typedef [t]. If [access] is provided as an argument
     then only members with the specified access_control are returned. *)
let typedef_get_members ?(access : access_control option) (t : trm) : (label * typ) list =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typedef_body with
    | Typedef_record rf ->
      List.fold_left (fun acc (rf, rf_ann) ->
        match rf with
        | Record_field_member (lb, ty) ->
          begin match access with
          | Some accs -> if accs = rf_ann then (lb, ty) :: acc else acc
          | None -> (lb, ty) :: acc
          end
        | Record_field_method _ -> acc
      ) [] (List.rev rf)
    | _ -> trm_fail t "Ast.typdef_get_members: this function should be called only for typedef structs and classes"
    end
  | _ -> trm_fail t "Ast.typedef_get_members: can't get members of a trm that's not a type definition."


(** [typedef_get_methods ~access t]: returns all the methods of typedef [t]. If [access] is provided as an argument
      then only methods with the specified access_control are returned. *)
let typedef_get_methods ?(access : access_control option) (t : trm) : trm list =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typedef_body with
    | Typedef_record rf ->
      List.fold_left (fun acc (rf, rf_ann) ->
        match rf with
        | Record_field_member _fm ->  acc
        | Record_field_method trm ->
          begin match access with
          | Some accss -> if accss = rf_ann then trm :: acc else acc
          | None -> trm :: acc
          end
      ) [] (List.rev rf)
    | _ -> trm_fail t "Ast.typdef_get_methods: this function should be called only for typedef structs and classes."
    end
  | _ -> trm_fail t "Ast.typedef_get_methods: can't get methods of a trm that's not a type definition. "

(** [typedef_get_all_fields t]: returns all the fields of [t]. *)
let typedef_get_all_fields (t : trm) : record_fields =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typedef_body with
    | Typedef_record rf -> rf
    | _ -> trm_fail t "Ast.typdef_get_all_fields: this function should be called only for structs and classes."
    end
  | _ -> trm_fail t "Ast.get_all_fields: only structs and classes have fields"


(** [get_member_type t rf]: returns the type of the member [rf]. *)
let get_member_type (t : trm) (rf : record_field) : typ =
  match rf with
  | Record_field_member (_, ty) -> ty
  | Record_field_method t1 ->
    begin match t1.desc with
    | Trm_let_fun (_, ty, _, _, _) -> ty
    | _ -> trm_fail t "Ast.get_member_type: can't get the type of the member [rf]."
    end
