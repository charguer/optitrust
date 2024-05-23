open Ast


(* **************************** Typ constructors *************************** *)

(* [typ_build ~annot ~attributes ~desc ()]: builds typ [ty] with its fields given as arguments. *)
let typ_build ~(annot : typ_annot list) ?(attributes : attribute list = []) ~(desc : typ_desc) () : typ =
  let ty = {typ_annot = annot; typ_attributes = attributes; typ_desc = desc} in
  (* Stats for types? *)
  ty

(* [typ_make ~annot ~attributes desc]: builds typ [ty] with the type description [desc] and other fields given. *)
let typ_make ?(annot : typ_annot list = []) ?(attributes = []) (desc : typ_desc) : typ =
  typ_build ~annot ~attributes ~desc ()

(* [typ_alter ~annot ~attributes ~desc ty]: alters any of the fields of [ty] that was provided as argument. *)
let typ_alter ?(annot : typ_annot list = []) ?(attributes = []) ?(desc : typ_desc option) (ty : typ) : typ =
  let annot = match annot with [] -> ty.typ_annot |_ -> annot in
  let attributes = match attributes with | [] -> ty.typ_attributes | _ -> attributes in
  let desc = match desc with | Some td -> td | None -> ty.typ_desc in
  typ_build ~annot ~attributes ~desc ()


(* [typ_repplace desc]: an alias of [typ_alter] to alter only thet description of the trm [ty]. *)
let typ_replace (desc : typ_desc) (ty : typ) : typ =
  typ_alter ~desc ty

(* [typ_const ~annot ~attributes t]: const type constructor *)
let typ_const ?(annot : typ_annot list = []) ?(attributes = [])
  (t : typ) : typ =
  (* DEPRECATED {typ_annot = annot; typ_desc = Typ_const t; typ_attributes} *)
  typ_make ~annot ~attributes (Typ_const t)

(** [typ_constr]: create a type constructor. *)
let typ_constr ?(annot : typ_annot list = []) ?(attributes = []) ?(tid : typconstrid = next_typconstrid ()) ?(tl : typ list = []) (name : typconstr) : typ =
  typ_make ~annot ~attributes (Typ_constr (name, tid, tl))

let typ_constr_inv (t : typ) : (typconstr * typconstrid * typ list) option =
  match t.typ_desc with
  | Typ_constr (name, tid, tl) -> Some (name, tid, tl)
  | _ -> None

(* [typ_auto ~annot ~attributes ()]: auto type constructor *)
let typ_auto ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_auto

(* [typ_unit ~annot ~attributes ()]: void type constructor *)
let typ_unit ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_unit


(* [typ_int ~annot ~attributes ()]: int type constructor *)
let typ_int ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_int

(* [typ_float ~annot ~attributes ()]: float type constructor *)
let typ_float ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_float

(* [typ_double ~annot ~attributes ()]: double type constructor *)
let typ_double ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_double

(* [typ_bool ~annot ~attributes ()]: bool type constructor *)
let typ_bool ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_bool

(* [typ_char ~annot ~attributes ()]: char type constructor *)
let typ_char ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_char


(* [typ_string ~annot ~attributes ()]: char type constructor *)
let typ_string ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_string

(* [typ_ptr ~annot ~attributes kind t]: pointer type constructor,
   Note: references are considered as pointer types in OptiTrust *)
let typ_ptr ?(annot : typ_annot list = []) ?(attributes = [])
  (kind : ptr_kind) (t : typ) : typ =
  typ_make ~annot ~attributes (Typ_ptr {ptr_kind = kind; inner_typ = t} )

(* [typ_array ~annot ~attributes t s]: array type constructor *)
let typ_array ?(annot : typ_annot list = []) ?(attributes = []) ?(size : trm option) (t : typ) : typ =
  typ_make ~annot ~attributes (Typ_array (t, size))

(* [typ_fun ~annot ~attributes args res]: function type constructor *)
let typ_fun ?(annot : typ_annot list = []) ?(attributes = [])
  (args : typ list) (res : typ) : typ =
  typ_make ~annot ~attributes (Typ_fun (args, res) )

(* [typ_record ~annot ~attributes rt name]: record type constructor *)
let typ_record ?(annot : typ_annot list = []) ?(attributes = [])
  (rt : record_type) (name : typ) : typ =
  typ_make ~annot ~attributes (Typ_record (rt, name) )

(* [typ_template_param ~annot ~attributes name]: template type constructor *)
let typ_template_param ?(annot : typ_annot list = []) ?(attributes = [])
  (name : string) : typ =
  typ_make ~annot ~attributes (Typ_template_param name )

(* [typ_ptr_generated ty]: generated pointer type constructor *)
let typ_ptr_generated (ty : typ) : typ =
  typ_ptr ~attributes:[GeneratedTyp] Ptr_kind_mut ty

(* [typedef_prod ~recursive field_list]: typedef kind constructor *)
let typdef_record (fields : record_fields) : typdef_body =
  Typdef_record fields

(* [typ_str ~annot ~attributes s] *)
let typ_str ?(annot : typ_annot list = []) ?(attributes = [])
  (s : code_kind) : typ =
  typ_make ~annot ~attributes (Typ_arbitrary s )

(* [typ_decl ~annot ~attributes expr]: type declaration based on an expression. *)
let typ_decl ?(annot : typ_annot list = []) ?(attributes = []) (expr : trm) =
  typ_make ~annot ~attributes (Typ_decl expr )

(* [typ_ref ~annot ~attributes ty]: alias to typ_ptr Ptr_kind_ref ty *)
let typ_ref ?(annot : typ_annot list = []) ?(attributes = [])
  (ty : typ) : typ =
  typ_ptr ~annot ~attributes Ptr_kind_ref ty

(* [typ_lref ~annot ~attributes ty]: alias to typ_ref (typ_ref ty). *)
let typ_lref ?(annot : typ_annot list = []) ?(attributes = [])
  (ty : typ) : typ =
    typ_ref ~annot ~attributes (typ_ref ty)

(*****************************************************************************)


(* [typ_ref_inv ty]: get the inner type of a reference *)
let typ_ref_inv (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_ref; inner_typ = ty1} -> Some ty1
  | _ -> None

(* [typ_ptr_inv ty]: get the inner type of a pointer *)
let typ_ptr_inv (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty1} -> Some ty1
  | _ -> None

let typ_array_inv (ty : typ) : (typ * trm option) option =
  match ty.typ_desc with
  | Typ_array (typ, size) -> Some (typ, size)
  | _ -> None

let typ_const_inv (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_const typ -> Some typ
  | _ -> None

let typ_const_ptr (ty : typ) : typ =
  typ_const (typ_ptr Ptr_kind_mut ty)

(* [typ_const_ptr_inv ty]: get the inner type of a constant pointer *)
let typ_const_ptr_inv (ty : typ) : typ option =
  Option.bind (typ_const_inv ty) typ_ptr_inv

let typ_const_array_inv (ty : typ) : (typ * trm option) option =
  Option.bind (typ_array_inv ty) (fun (ty2, size) ->
    Option.map (fun ty3 -> (ty3, size)) (typ_const_inv ty2))

(* [typ_add_attribute att ty]: adds the attribute [att] to the type [ty] *)
let typ_add_attribute (att : attribute)(ty : typ) : typ =
  {ty with typ_attributes = att :: ty.typ_attributes}

(* [typ_has_attribute att ty]: checks if [ty] has attribute [att]. *)
let typ_has_attribute (att : attribute) (ty : typ) : bool =
  List.mem att ty.typ_attributes


(*****************************************************************************)


let typ_inv ?(error : string = "") (trm : trm) (k : typ -> 'a option) (t : typ) : 'a =
  match k t with
  | None -> if error = "" then assert false else trm_fail trm error
  | Some r -> r

(* [is_generated_typ ty]: checks ia a typ is a type used only for optitrust encoding *)
let is_generated_typ (ty : typ) : bool =
  List.mem GeneratedTyp ty.typ_attributes


(* [is_atomic_typ ty]: checks if [ty] is an atomic type *)
  let is_atomic_typ (ty : typ) : bool =
    match ty.typ_desc with
    | Typ_int | Typ_unit | Typ_float | Typ_double | Typ_bool | Typ_char |Typ_string -> true
    | _ -> false

  (* [get_typ_kind ctx ty]: based on the context [ctx], get the kind of type [ty] *)
  let rec get_typ_kind (ctx : typ_ctx) (ty : typ) : typ_kind =
    if is_atomic_typ ty then Typ_kind_basic ty.typ_desc
      else
    match ty.typ_desc with
    | Typ_const ty1 -> get_typ_kind ctx ty1
    | Typ_ptr rf when rf.ptr_kind = Ptr_kind_ref -> Typ_kind_reference
    | (Typ_ptr _| Typ_array _) -> Typ_kind_array
    | Typ_fun _ -> Typ_kind_fun
    | Typ_var _ -> Typ_kind_var
    | Typ_constr (_, tid, _) ->
       let td_opt = Typ_map.find_opt tid ctx.ctx_typedef in
       begin match td_opt with
       | None -> Typ_kind_undefined
       | Some td ->
           begin match td.typdef_body with
          | Typdef_alias ty1 -> get_typ_kind ctx ty1
          | Typdef_record _ -> Typ_kind_record
          | Typdef_sum _| Typdef_enum _ -> Typ_kind_sum
          end
       end
    | _ -> Typ_kind_basic ty.typ_desc

  (* [get_inner_ptr_type ty]: gets the underlying type of [ty] when [ty] is a generated pointer type *)
  let get_inner_ptr_type (ty : typ) : typ =
    match ty.typ_desc with
    | Typ_ptr {inner_typ = ty1;_} when is_generated_typ ty -> ty1
    | _ -> ty

  (* [get_inner_array_type ty]: returns the underlying type of [ty] when [ty] is an array type. *)
  let get_inner_array_type (ty : typ) : typ =
    match ty.typ_desc with
    | Typ_array (ty, _) -> ty
    | _ -> ty


  (* [get_inner_const_type ty]: gets the underlying type of [ty] when [ty] is a const type *)
  let get_inner_const_type (ty : typ) : typ =
    match ty.typ_desc with
    | Typ_const ty -> ty
    | _ -> ty

  (* [get_inner_type ty]: returns the inner type of [ty] when [ty] is a pointer type, const type or an array type. *)
  let get_inner_type (ty : typ) : typ =
    match ty.typ_desc with
    | Typ_const ty -> ty
    | Typ_ptr {inner_typ = ty; _} -> ty
    | Typ_array (ty, _) -> ty
    | _ -> ty


  (* [decl_type t]: returns the type of declaration [t]. *)
  let decl_type (t : trm) : typ option =
    match t.desc with
    | Trm_let ((_, tx), _) -> Some (get_inner_ptr_type tx)
    | Trm_let_fun (_, ty, _, _, _) -> Some ty
    | _ -> None


  (* [is_reference]: checks if the type is a reference type or not *)
  let is_reference (ty : typ) : bool =
    let ty = get_inner_ptr_type ty in
    match ty.typ_desc with
    | Typ_ptr {ptr_kind = Ptr_kind_ref;_} -> true
    | _ -> false

  (* [is_typ_const ty]: checks if [ty] is a const type *)
  let is_typ_const (ty : typ) : bool =
    match ty.typ_desc with
    | Typ_const _ -> true
    (* | Typ_array (ty, s) -> is_typ_const ty *)
    | _ -> false



(* [is_type_unit t]: checks if the [t] has type void *)
let is_type_unit (t : typ) : bool =
  match t.typ_desc with
  | Typ_unit -> true
  | _ -> false

(* [is_lit t]: checks if [t] is a literal or not *)
let is_lit (t : trm) : bool =
  match t.desc with
  | Trm_val (Val_lit _) -> true
  | _ -> false

(* [is_typ_ptr ty]: checks if [ty] is a pointer type *)
let is_typ_ptr (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_mut;_} -> true
  | _ -> false

(* [is_typ_ref ty]: checks if [ty] is a reference type *)
let is_typ_ref (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_ref;_} -> true
  | _ -> false

(* [is_typ_fun ty]: checks if [ty] is a function type *)
let is_typ_fun (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_fun _ -> true | _ -> false

(* [is_typ_struct struct_name ty]: checks if [ty] is a constructed struct type *)
let is_typ_struct (struct_name : Qualified_name.t) (ty_opt : typ option) : bool =
  match ty_opt with
  | Some ty ->
    begin match ty.typ_desc with
    | Typ_constr (sn, _, _) -> sn = struct_name
    | _ -> false
    end
  | None -> false



(* [is_typ ty]: checks if [ty] is a proper type *)
let is_typ (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_arbitrary _ -> false
  | _ -> true


(* [typ_align align ty]: adds the alignas attribute to type ty *)
let typ_align (align : trm) (ty : typ) =
  typ_add_attribute (Alignas align) ty


(* [is_typ_array ty]: checks if [ty] is of type array. *)
  let is_typ_array (ty : typ) : bool =
    match ty.typ_desc with
    | Typ_array _ -> true
    | _ -> false

let typconstr_has_name ((namespaces, name) : typconstr) (n : string) : bool =
  namespaces = [] && name = n

(* ********************************************************************************************** *)


(* [typ_map f ty]: applies f on type ty recursively *)
let typ_map (f : typ -> typ) (ty : typ) : typ =
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  match ty.typ_desc with
  | Typ_ptr {ptr_kind= pk; inner_typ = ty} -> typ_ptr ~annot ~attributes pk (f ty)
  | Typ_array (ty, size) -> typ_array ~annot ~attributes (f ty) ?size
  | Typ_fun (tyl, ty) ->
     typ_fun ~annot ~attributes (List.map f tyl) (f ty)
  (* var, unit, int, float, double, bool, char *)
  | _ -> ty



(* [same_types ~match_generated_start typ_1 typ_2]: checks if two types are the same *)
  let rec same_types ?(match_generated_star : bool = false) (typ_1 : typ) (typ_2 : typ) : bool =
    let aux = same_types ~match_generated_star in
    (typ_1.typ_annot = typ_2.typ_annot) && (
      match typ_1.typ_desc, typ_2.typ_desc with
      | Typ_const typ_a1, Typ_const typ_a2 ->
        (aux typ_a1 typ_a2)
      | Typ_var (_, id1), Typ_var (_, id2) ->
        id1 = id2
      | Typ_constr (_, id1, tl1), Typ_constr (_, id2, tl2) ->
        (id1 = id2) && (tl1 = tl2)
      | Typ_unit, Typ_unit -> true
      | Typ_int, Typ_int -> true
      | Typ_float, Typ_float -> true
      | Typ_double, Typ_double -> true
      | Typ_bool, Typ_bool -> true
      | Typ_char, Typ_char -> true
      | Typ_string, Typ_string -> true
      | Typ_ptr {ptr_kind = pk1; inner_typ = typ_a1}, Typ_ptr {ptr_kind = pk2; inner_typ = typ_a2} ->
       if match_generated_star then (pk1 = pk2) && (is_generated_typ typ_1 && is_generated_typ typ_2) && (aux typ_a1 typ_a2)
        else (not (is_generated_typ typ_1 || is_generated_typ typ_2)) && (pk1 = pk2) && (aux typ_a1 typ_a2)
      | Typ_array (typa1, size1), Typ_array (typa2, size2) ->
          (same_types typa1 typa2) && (size1 = size2)
      | _, _ -> false)

let typ_of_get (t : typ) : typ option =
  match t with
  | { typ_desc = Typ_array (ty, _); _} -> Some ty
  | { typ_desc = Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty}; _} -> Some ty
  | { typ_desc = Typ_const { typ_desc = Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty}; _}; _ } -> Some ty
  | _ -> None
