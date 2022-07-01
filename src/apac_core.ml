open Ast
open Target


(* [use_goto_for_return_aux mark t]: transforms the body of the funciton declaration in such a way that
      all return statements are replaced with gotos,
    [mark] - mark used to mark the introduced sequence.
    [t] - ast of the function definition. *)
let use_goto_for_return_aux (mark : mark) (t : trm) : trm =
  match t.desc with 
  | Trm_let_fun (qn, ret_ty, args, body) -> 
    let seq_to_insert, _ = Internal.replace_return_with_assign ~check_terminal:false ~exit_label:"__exit" "__res" body in 
    let new_body = 
      begin match ret_ty.typ_desc with 
      | Typ_unit -> 
        trm_add_mark mark (trm_seq_nomarks [
          seq_to_insert;
          trm_add_label "__exit" (trm_unit())])
      | _ -> 
        let new_decl = trm_let_mut ("__res", ret_ty) (trm_uninitialized ()) in 
        trm_add_mark mark (trm_seq_nomarks [
          new_decl;
          seq_to_insert;
          trm_add_label "__exit" (trm_unit());
          trm_ret (Some (trm_var_get "__res"))
        ])
      end in 
      trm_alter ~desc:(Some (Trm_let_fun (qn, ret_ty, args, new_body))) t
  | _ -> fail t.loc "Apac_core.use_goto_for_return_aux: expected  a target to a function definition."


(* [use_goto_for_return mark t p]: applies [use_goto_for_return_aux] at the trm [t] with path [p]. *)
let use_goto_for_return (mark : mark) : Transfo.local =
  apply_on_path(use_goto_for_return_aux mark)


(* [dep_kind]: type used for [arg_dep]. *)
type dep_kind = 
  | Dep_kind_in 
  | Dep_kind_out
  | Dep_kind_inout
  | Dep_kind_outin
  | Dep_kind_sink
  | Dep_kind_source

(* [arg_dep]: a record that stored all the dependecy informations about one argument. *)
type arg_dep = {
  arg_dep_var : var;
  arg_dep_typ : typ;
  arg_dep_kind : dep_kind;
}

(* [arg_deps]: a list of arg_dep. *)
type arg_deps = arg_dep list


(* [sorted_arg_deps]: a record used for storing classified arguments based on their respective dependency kind. *)
type sorted_arg_deps = {
  dep_in : deps;
  dep_out : deps;
  dep_inout : deps;
  dep_outin : deps;
  (* LATER: add other fields. *)
}


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


(* [get_arg_dependencies t]: for each argument of the function [t] returns all the dependencies.  *)
let get_arg_dependencies (t : trm) : arg_deps =
  match Function_core.get_function_prototype t with 
  | Some (ty, args) -> 
    List.map (fun (x, ty) -> 
      { arg_dep_var = x;
        arg_dep_typ = ty;
        arg_dep_kind = dep_kind_of_typ ty }
     ) args
  | None -> fail t.loc "Ast.get_arg_dependencies: expected a function definition"


(* TODO: Michel
    [sort_arg_dependencies arg_deps]: classifies all the argument dependencies into four categories.
    In, Out, Inout, Outin. *)
let sort_arg (arg_deps : arg_deps) : sorted_arg_deps = 
 { dep_in = [];
   dep_out = [];
   dep_inout = [];
   dep_outin = [];}


(* [get_constified_arg_aux ty]: return the constified typ of the typ [ty]*)
let rec get_constified_arg_aux (ty : typ) : typ =
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  match ty.typ_desc with 
  | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty} ->
    typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (get_constified_arg_aux ty))
  | Typ_const {typ_desc = Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty }; typ_annot = annot; typ_attributes = attributes} ->
    typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (get_constified_arg_aux ty))
  | Typ_constr (_, id, _) ->
    begin match Context.typid_to_typedef id with
    | Some td -> 
      begin match td.typdef_body with 
      | Typdef_alias ty -> get_constified_arg_aux ty 
      | _ -> typ_const ty
      end
    | None -> typ_const ty
    end
  | Typ_const _ -> ty
  | _ -> typ_const ty

(* [get_constified_arg ty]: applies [get_constified_arg_aux] at the typ [ty] or the typ pointer by [ty] 
      if [ty] is a reference or a rvalue reference 
      return the constified typ of [ty]  *)
let get_constified_arg (ty : typ) : typ =
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  match ty.typ_desc with 
  | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
    begin match ty.typ_desc with
    (* rvalue reference *)
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
      typ_lref ~annot ~attributes (get_constified_arg_aux ty)
    (* reference *)
    | _ -> typ_ref ~annot ~attributes (get_constified_arg_aux ty)
    end
  | _ -> get_constified_arg_aux ty

(* [constify_args t]: transforms the type of arguments of the function declaration in such a way that
      "const" keywords are added whereever it is possible.
    [is_const] - list of booleans that tells if the argument should be constify. Its length must be the number of arguments.
    [t] - ast of the function definition. *)
let constify_args_aux (is_const : bool list) (t : trm) : trm =
  match t.desc with
  | Trm_let_fun (qvar, ret_typ, args, body) -> 
    let is_const = if is_const = [] then List.init (List.length args) (fun _ -> true) else is_const in
    let const_args = (List.map2 (fun (v, ty) b -> 
      if b then (v, (get_constified_arg ty)) else (v, ty)
      ) args is_const) in
    trm_let_fun ~annot:t.annot ~loc:t.loc ~ctx:t.ctx ~qvar "" ret_typ const_args body
  | _ -> fail t.loc "Apac_core.constify_args expected a target to a function definition."

let constify_args (is_const : bool list) : Transfo.local =
  apply_on_path(constify_args_aux is_const)