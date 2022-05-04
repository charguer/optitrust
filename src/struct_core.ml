open Ast
open Target


(* [set_explicit_aux t]: transform an assigment into a list of field assignments
    params: 
     [t]: ast of the assignment *)
let set_explicit_aux (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, [lt; rt]) ->
    (* Temporary hack for overloaded set operator *)
    let lt = begin match trm_prim_inv f with
      | Some (Prim_overloaded_op (Prim_binop Binop_set)) ->
        get_operation_arg lt
      | _ -> lt
      end
     in
    let tid_r = Internal.get_typid_from_trm rt  in
      let tid_l = Internal.get_typid_from_trm lt  in
      let tid = match tid_r, tid_l with
      | -1, _ -> tid_l
      | _, -1 -> tid_r
      | _, _ -> if tid_r = tid_l then tid_r
                  else fail t.loc "Struct_core.set_explicit_aux: different types in an assignment"
      in
      let struct_def =
        if tid <> -1 then
          match Context.typid_to_typedef tid with
          | Some td -> td
          | _ -> fail t.loc "Struct_core.set_explicit_aux: could not get the declaration of typedef"
        else begin
          Printf.printf "%s\n" (AstC_to_c.ast_to_string t);
          fail t.loc "Struct_core.set_explicit_aux: explicit assignment cannot operate on unknown types"
        end
      in
      let field_list = Internal.get_field_list struct_def in
      begin match rt.desc with
      | Trm_apps (f1, [rt1]) when is_get_operation rt ->
         let exp_assgn = List.mapi (fun i (sf, ty) ->
          trm_set (trm_struct_access ~typ:(Some ty) lt sf) {rt with desc = Trm_apps (f1, [trm_struct_access ~typ:(Some ty) rt1 sf]); typ = Some ty}
         ) field_list in
         trm_seq_no_brace exp_assgn
      | Trm_struct st ->
        let st = Mlist.to_list st in
        let exp_assgn = List.mapi (fun i (sf, ty) ->
          trm_set (trm_struct_access ~typ:(Some ty) lt sf) (List.nth st i)
        ) field_list
         in
        trm_seq_no_brace exp_assgn
      | _ ->  (* other cases are included here *)
        let exp_assgn = List.mapi (fun i (sf, ty) ->
         trm_set (trm_struct_access ~typ:(Some ty) lt sf) (trm_struct_get ~typ:(Some ty) rt sf)
         ) field_list in
         trm_seq_no_brace exp_assgn
      end
  | _ -> fail t.loc "Struct_core.set_explicit_aux: expected a set operation"

(* [set_explicit t p]: apply [set_explicit_aux] at trm [t] with path [p] *)
let set_explicit : Transfo.local =
  apply_on_path(set_explicit_aux )


(* [set_implicit t]: transform a sequence with a list of explicit field assignments into a single assignment.
    pararms:
      [t]: ast of the sequence containing the assignments *)
let set_implicit_aux (t: trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     let rhs_trms = Mlist.fold_left ( fun acc instr ->
      match instr.desc with
      | Trm_apps (_, [_;rhs]) ->
        begin match rhs.desc with
        | Trm_apps(f', [rt])  ->
          begin match f'.desc with
          | Trm_val ( Val_prim ( Prim_unop Unop_get ) ) ->
            begin match rt.desc with
              | Trm_apps(f'',[rt]) ->
               begin match f''.desc with
               | Trm_val (Val_prim (Prim_unop (Unop_struct_access _)))
               | Trm_val (Val_prim (Prim_unop (Unop_struct_get _)))->
                  [trm_get rt]
               | _ -> fail f'.loc "Struct_core.set_implicit_aux: expected a struct acces on the right hand side of the assignment"
               end
              | _ -> fail f'.loc "Struct_core.set_implicit_aux: expected a trm_apps"
            end
            | Trm_val (Val_prim (Prim_unop (Unop_struct_access _)))
            | Trm_val (Val_prim (Prim_unop (Unop_struct_get _)))->
                  [rt]
            | _ -> fail f'.loc "Struct_core.set_implicit_aux: expected a struct acces on the right hand side of the assignment"
           end
          | _ -> acc @ [rhs]
          end
      | _ -> fail t.loc "Struct_core.set_implicit_aux: expected a set operation"
    ) [] tl in
    let first_instruction = Mlist.nth tl 0 in
    begin match first_instruction.desc with
    | Trm_apps(f,[lhs;_]) ->
          begin match f.desc with
          | Trm_val ( Val_prim ( Prim_binop Binop_set) ) ->
            let lt = begin match lhs.desc with
            | Trm_apps(f', [lt]) ->
              begin match f'.desc with
              | Trm_val (Val_prim (Prim_unop (Unop_struct_access _)))
              | Trm_val (Val_prim (Prim_unop (Unop_struct_get _)))-> lt
              | _ -> fail f'.loc "Struct_core.set_implicit_aux: expected a struct access on the left hand side of the assignment"
              end
            | _ -> fail lhs.loc "Struct_core.set_implicit_aux: expected a struct access"
            end
            in
            begin match rhs_trms with
            | [rhs1] -> trm_set lt rhs1
            | _ -> trm_set lt (trm_struct (Mlist.of_list rhs_trms))
            end
          | _ -> fail f.loc "Struct_core.set_explicit_aux: expected an assignment instruction"
          end
      | _ -> fail t.loc "Struct_core.set_implicit_aux: expected a sequence with all explicit assignments"

    end
  | _ -> fail t.loc "Struct_core.set_implicit_aux: sequence which contains the set instructions was not matched"

(* [set_implicit keep_label t p]: apply [set_implicit_aux] at trm [t] with path [p] *)
let set_implicit (keep_label : bool) : Transfo.local =
  apply_on_path (Internal.apply_on_path_targeting_a_sequence ~keep_label (set_implicit_aux) "set_implicit")


(* [inline_struct_accesses x t]: change all the occurrences of the struct accesses to a field into a field
    params:
      [x]: the name of the field for which the transformation is applied
      [t]: ast node located in the same level as the stract declaration or deeper *)
let inline_struct_accesses (x : var) (t : trm) : trm =
  let rec aux (outer_field : string) (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, base) ->
      begin match f.desc with
      | Trm_val (Val_prim (Prim_unop (Unop_struct_access z))) ->
        begin match base with
        | [base'] ->
          if contains_field_access x base'
            then aux z base'
            else if outer_field <> "" then
              let updated_field = Convention.name_app z outer_field in
              trm_struct_access base' updated_field
            else trm_map (aux "") t
        | _ -> fail f.loc "Struct_core.inline_struct_access: suspicious struct access"
        end
      | Trm_val (Val_prim (Prim_unop (Unop_struct_get z))) ->
        begin match base with
        | [base'] ->
          if contains_field_access x base'
            then aux z base'
            else if outer_field <> "" then
              let updated_field = Convention.name_app z outer_field in
              trm_struct_get base' updated_field
            else trm_map (aux "") t
        | _ -> fail f.loc "Struct_core.inline_struct_access: suspicious struct access"
        end
      | _ -> trm_map (aux outer_field) t
      end
    | _ -> trm_map (aux outer_field) t

   in aux "" t

(* [inline_struct_initialization struct_name field_list field_index t]: change all struct in struct initializations
    params:
      [struct_name]: the type of the struct that is being inlined
      [field_list]: a list of fields from the original type of the struct
      [field_index]: index of the field in the outer struct
      [t]: ast node located in the same level as the main struct declaration or deeper *)
let inline_struct_initialization (struct_name : string) (field_list : field list) (field_index : int) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    (* Searching for struct intialization lists of type typedef struct {} struct_name *)
    | Trm_struct term_list ->
      begin match t.typ with
      | Some ty ->
        let ty = get_inner_const_type ty in
        begin match ty.typ_desc with
        | Typ_constr (y, _, _) when y = struct_name ->
          let lfront, trm_to_change, lback = Internal.get_trm_and_its_relatives field_index term_list in
          begin match trm_to_change.desc with
          | Trm_struct sl ->
            let new_term_list = Mlist.merge lfront sl in
            let new_term_list = Mlist.merge new_term_list lback  in
            trm_struct ~annot:t.annot ~typ:t.typ ~marks:t.marks new_term_list
          | Trm_apps (_, [{desc = Trm_var (_, p);_} as v]) when is_get_operation trm_to_change ->
            let sl = List.map (fun f -> trm_get (trm_struct_access (trm_var ~typ:v.typ p) f)) field_list in
            let new_term_list = Mlist.merge lfront (Mlist.of_list sl) in
            let new_term_list = Mlist.merge new_term_list lback in
            trm_struct ~annot:t.annot ~typ:t.typ ~marks:t.marks new_term_list

          | Trm_var (_, p) ->
            let sl = List.map (fun f -> trm_struct_get (trm_var ~typ:t.typ p) f) field_list in
            let new_term_list = Mlist.merge lfront (Mlist.of_list sl) in
            let new_term_list = Mlist.merge new_term_list lback in
            trm_struct ~annot:t.annot ~typ:t.typ ~marks:t.marks new_term_list

          | _ -> fail t.loc "Struct_core.inline_struct_initialization: struct intialization list is not compatible with definition"
          end
        | _ -> trm_map aux t
        end
      | _ -> fail t.loc "Struct_core.inline_struct_initialization: couldn't find the type of the struct intitialization type, try reparsing first"
        end
    | _ -> trm_map aux t
  in aux t

(* [reveal_field_aux field_to_reveal index t]: reveal field [field_to_reveal] on its typedef struct definition
     update all the initializations and accesses according to this change
    params:
      [field_to_reveal]: field that is going to be revealed
      [index]: index of the struct declaration inside the sequence it belongs to
      [t]: trm corresponding to a typedef struct definition *)
let reveal_field_aux (field_to_reveal : field) (index : int) (t : trm ) =
  match t.desc with
  | Trm_seq tl ->
    let lfront, td, lback =  Internal.get_trm_and_its_relatives index tl in
    begin match td.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with
      | Typdef_prod (t_names, field_list) ->
       let field_index = Internal.get_field_index field_to_reveal field_list in
       let lfront1, lback1 = Xlist.split_at field_index field_list in
       let field_to_inline1, lback1 = if List.length lback1 = 1 then (lback1, []) else
        Xlist.split_at 1 lback1 in
       let _ ,field_type = List.nth field_to_inline1 0 in
       let tyid = begin match field_type.typ_desc with
       | Typ_constr (_, tid , _) -> tid
       | Typ_array (ty1, _) ->
        begin match ty1.typ_desc with
        | Typ_constr (_, tid, _) -> tid
        | _ -> fail t.loc "Struct_core.reveal_field_aux: expected a typ_constr"
        end
       | _ -> fail t.loc "Struct_core.reveal_field_aux: expected a typ_constr"
       end
       in
       let struct_def =
        if tyid <> -1 then match Context.typid_to_typedef tyid with
          | Some td -> td
          | _ -> fail t.loc "Struct_core.set_explicit_aux: could not get the declaration of typedef"
        else
          fail t.loc "Struct_core.set_explicit_aux: explicit assignment is supported only for struct types"
       in
       let inner_type_field_list = begin match struct_def.typdef_body with
        | Typdef_prod (_, s) -> s
        | _ -> fail t.loc "Struct_core.reveal_field_aux: the field wanted to inline should have also a struct typedef"
        end
       in
       let inner_type_field_list = List.map (fun (x, typ) ->
            let new_field = Convention.name_app field_to_reveal x in
            match field_type.typ_desc with
            | Typ_array (_, size) -> (new_field, (typ_array typ size))
            | _ -> (new_field, typ)) inner_type_field_list in

       let field_list = (lfront1 @ inner_type_field_list @ lback1) in
       let new_typedef = {td with typdef_body =  Typdef_prod (t_names, field_list)} in
       let new_trm = trm_typedef new_typedef in
       let lback = Mlist.map (inline_struct_accesses field_to_reveal) lback in
       let lback = Mlist.map (inline_struct_initialization td.typdef_tconstr (fst (List.split (Internal.get_field_list struct_def))) field_index) lback in
       let new_tl = Mlist.merge lfront lback in
       let new_tl = Mlist.insert_at index new_trm new_tl in
       { t with desc = Trm_seq new_tl}
      | _ -> fail t.loc "Struct_core.reveal_field_aux: expected a struct "
      end
    | _ -> fail t.loc "Struct_core.reveal_field_aux: expected a trm_typedef"
    end
  | _ -> fail t.loc "Struct_core.reveal_field_aux: expected the surrounding sequence"

(* [reveal_field field_to_reveal index t p]: apply [reveal_field] at trm [t] with path [p] *)
let reveal_field (field_to_reveal : field) (index : int) : Transfo.local =
  apply_on_path (reveal_field_aux field_to_reveal index)

(* [fields_reorder_aux struct_fields move_where around t]: reorder fields of a struct
     params:
       [struct_fields]: a list of fields to move
       [move_where]: a string which is equal either "before" or "after"
       [around]: the target field where fields are going to be moved to
       [t]: ast of the typedef struct *)
let fields_reorder_aux (struct_fields: vars) (move_where : reorder) (t: trm) : trm =
  match t.desc with
  | Trm_typedef td ->
   begin match td.typdef_body with
   | Typdef_prod (tn, fs) ->
    let field_list = Internal.reorder_fields move_where struct_fields fs in
   trm_typedef {td with typdef_body = Typdef_prod (tn, field_list)}
  | _ -> fail t.loc "Struct_core.fields_reorder_aux: expected a typdef_prod"
  end
  | _ -> fail t.loc "Struct_core.fields_reorder_aux: expected a typedef definiton"

(* [fields_reorder struct_fields move_where around t p]: apply [fields_reorder_aux] at trm [t] with path [p] *)
let fields_reorder (struct_fields : vars) (move_where : reorder) : Transfo.local =
  apply_on_path(fields_reorder_aux struct_fields move_where)

(* [inline_struct_accesses name field t]: transform a specific struct access into a variable occurrence
    params:
      [name]: name of the variable to replace the struct access
      [field]: struct accesses on this field are going to be replaced with [name]
      [t]: ast node located in the same level as the variable declaration *)
let inline_struct_accesses (name : var) (field : var) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    begin match t.desc with
    | Trm_apps (f, [base]) ->
      begin match f.desc with
      | Trm_val (Val_prim (Prim_unop (Unop_struct_access y)))
        | Trm_val (Val_prim (Prim_unop (Unop_struct_get y))) when y = field ->
          begin match base.desc with
          | Trm_var (_, v) when v = name ->
            trm_var (Convention.name_app name field)
          | _ -> trm_map aux t
          end
      | _ -> trm_map aux t
      end
    | _ -> trm_map aux t
    end
   in aux t

(* [to_variables_aux index t]: change a variable declaration of type typedef struct into a list
      of variable declarations with types inherited from the fields of the underlying type.
    params:
      [index]: index of the declaration inside the sequence it belongs to
      [t]: ast of the surrounding sequence of the variable declarations *)
let to_variables_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, trm_to_change, lback = Internal.get_trm_and_its_relatives index tl in
    begin match trm_to_change.desc with
    | Trm_let (_, (x, tx), init) ->

      let typid = begin match (get_inner_ptr_type tx).typ_desc with
                  | Typ_constr (_, tid, _) -> tid
                  | _ -> fail t.loc "Struct_core.struct_to_variables_aux: expected a struct type"
                  end in
      let struct_def =
        if typid <> -1 then match Context.typid_to_typedef typid with
          | Some td -> td
          | _ -> fail t.loc "Struct_core.to_variables_aux: could not get the declaration of typedef"
        else
          fail t.loc "Struct_core.to_variables_aux: explicit assignment is supported only for struct types"
       in
      let field_list = Internal.get_field_list struct_def in
      let struct_init_list = begin match init.desc with
                             | Trm_apps(_, [base]) ->
                              begin match base.desc with
                              | Trm_struct ls -> (Mlist.to_list ls)
                              | _ -> fail init.loc "Struct_core.struct_to_variables_aux: expected a struct initialisation"
                              end
                             | Trm_struct ls -> (Mlist.to_list ls)
                             | _ -> []
                             end in
      let var_decls = List.mapi( fun  i (sf, ty) ->
          let new_name = Convention.name_app x sf in
          match struct_init_list with
          | [] -> trm_let_mut (new_name, ty) (trm_uninitialized ())
          | _ -> trm_let_mut (new_name, ty) (List.nth struct_init_list i)
      ) field_list in
      (* LATER: Optimize, a single pass over lback.items *)
      let lback = Mlist.map (fun t1 ->
        List.fold_left (fun t2 f1 ->
          inline_struct_accesses x f1 t2
        ) t1 (fst (List.split field_list))
      ) lback in
      let new_tl = Mlist.merge lfront (Mlist.of_list var_decls) in
      let new_tl = Mlist.merge new_tl lback in
      trm_seq ~annot:t.annot ~marks:t.marks new_tl

   | _ -> fail trm_to_change.loc "Struct_core.struct_to_variables_aux: expected a variable declaration"
    end
  | _ -> fail t.loc "Struct_core.struct_to_variables_aux: expected the surrounding sequence"

(* [to_variables index t p]: apply [to_variables_aux] at trm [t] with path [p] *)
let to_variables (index : int) : Transfo.local =
  apply_on_path (to_variables_aux index)

(* [Rename]: a module used for renaming the struct fields *)
module Rename = struct
  type t = string -> string
  let add_prefix (s : string) : t =
    fun str -> s ^ str

  let only_for (pattern : string) : t -> t =
    fun tr s ->
      if Tools.pattern_matches pattern s then tr s else s
end

(* [rename]: instantiation of module [Rename] *)
type rename = Rename.t

(* [rename_struct_accesses struct_name renam t]: rename all struct accesses based on [rename]
    params:
      [struct_name]: the constructed type whose fields are going to be renamed
      [rename]: a type used to rename the struct fields
      [t]: any node in the same level as the struct declaration *)
let rename_struct_accesses (struct_name : var) (rename : rename) (t : trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    begin match t.desc with
    | Trm_apps (f, [base]) ->
      begin match f.desc with
      | Trm_val (Val_prim (Prim_unop (Unop_struct_access y))) ->
          begin match base.typ with
          | Some ty ->
            begin match ty.typ_desc with
            | Typ_constr (x, _, _) when x = struct_name->
              trm_apps ~annot:t.annot ~typ:t.typ ~marks:t.marks ({f with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access (rename y))))})  [base]
            | _ -> trm_map (aux global_trm) t
            end
          | None -> trm_map (aux global_trm) t
          end
      | Trm_val (Val_prim (Prim_unop (Unop_struct_get y))) ->
        begin match base.typ with
          | Some ty ->
            begin match ty.typ_desc with
            | Typ_constr (x, _, _) when x = struct_name->
              trm_apps ~annot:t.annot ~typ:t.typ ~marks:t.marks ({f with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get (rename y))))})  [base]
            | _ -> trm_map (aux global_trm) t
            end
          | None -> trm_map (aux global_trm) t
          end
      | _ -> trm_map (aux global_trm) t
      end
    | _ -> trm_map (aux global_trm) t
    end
   in aux t t

(* [rename_fields_aux index rename t]: rename struct fields in the typedef struct definitions
    params:
      [index]: the index of the struct declaration in the sequence [t]
      [rename]: a type used to rename the fields
      [t]: the ast of the sequence which contains the struct declaration *)
let rename_fields_aux (index : int) (rename : rename) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, tdef , lback = Internal.get_trm_and_its_relatives index tl in
    begin match tdef.desc with
    | Trm_typedef ({typdef_tconstr = name; typdef_body = Typdef_prod (tn, fl);_}  as td) ->
        let new_fl = List.map (fun (x, ty) -> (rename x, ty)) fl in
        let new_tdef = trm_typedef ~annot:tdef.annot ~marks:tdef.marks {td with typdef_body = Typdef_prod (tn, new_fl)} in
        let lback = Mlist.map (fun t1 -> rename_struct_accesses name rename t1) lback in
        let new_tl = Mlist.merge lfront lback in
        let new_tl = Mlist.insert_at index new_tdef new_tl in
        trm_seq ~annot:t.annot ~marks:t.marks new_tl
    | _ -> fail tdef.loc "Struct_core.reanme_fields_aux: expected a typedef declaration"
    end
  | _ -> fail t.loc "Struct_core.rename_fields_aux: expected the sequence which contains the typedef declaration"

(* [rename_fields index rename t p]: apply [rename_aux] at trm [t] with path [p] *)
let rename_fields (index : int) (rename : rename) : Transfo.local =
  apply_on_path (rename_fields_aux index rename)

(* [update_fields_type_aux pattern ty t]: change the current type for all the struct fields
      that are matched with [pattern] and whose type can be changed by [typ_update]
    params:
      [pattern]: regular expression to match struct fields
      [typ_update]: function that modifies only specific types
      [t]: the ast of the typedef definition *)
let update_fields_type_aux (pattern : string ) (typ_update : typ -> typ) (t : trm) : trm =
  match t.desc with
  | Trm_typedef ({typdef_body = Typdef_prod (tn, fl);_}  as td) ->

      let update_type ty = typ_map typ_update ty in

      (* let rec update_type (ty_to_update : typ) : typ =
        match ty_to_update.typ_desc with
        | Typ_array _ | Typ_ptr _
          | Typ_const _ -> typ_map update_type ty_to_update
        | _ -> ty
        in  *)
      let replace_type (s : string) (ty1 : typ) : typ =
        if Tools.pattern_matches pattern s then (update_type ty1)  else ty1 in
      let new_fl = List.map (fun (x, ty2) -> (x, replace_type x ty2)) fl in
      trm_typedef ~annot:t.annot ~marks:t.marks {td with typdef_body = Typdef_prod (tn, new_fl)}
    | _ -> fail t.loc "Struct_core.reanme_fields_aux: expected a typedef declaration"

(* [update_fields_type pattern typ_update t p]: apply [update_fields_type_aux] at trm [t] with path [p] *)
let update_fields_type (pattern : string) (typ_update : typ -> typ) : Transfo.local =
  apply_on_path (update_fields_type_aux pattern typ_update )


(* [simpl_proj_aux t]: transform all expression of the form {1, 2, 3}.f into the trm it projects to
    params:
      [t]: ast of the node whose descendants can contain struct initialization list projections *)
let simpl_proj_aux (t : trm) : trm =
  (* Printf.printf "%s\n" (Ast_to_text.ast_to_string t); *)
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, [struct_list]) ->
      begin match trm_prim_inv f with
      | Some (Prim_unop (Unop_struct_get x)) | Some (Prim_unop (Unop_struct_access x))->
        begin match struct_list.desc with
        | Trm_struct tl ->
          let tid = Internal.get_typid_from_trm struct_list in
            if tid <> -1 then begin
              let struct_def = match Context.typid_to_typedef tid with
              | Some td -> td
              | _ -> fail struct_list.loc "Struct_core.simpl_proj_aux: couldn't retrieve the the struct declaration" in
              let field_list = Internal.get_field_list struct_def in
              let field_vars = fst (List.split field_list) in
              match Xlist.index_of x field_vars  with
              | Some i ->
                Mlist.nth tl i
              | _ -> t
              end
              else  t
        | _ -> trm_map aux t
        end
      | _ -> trm_map aux t
      end
    | _ -> trm_map aux t
   in aux t

(* [simpl_proj t p]: apply [simpl_proj_aux] at trm [t] with path [p] *)
let simpl_proj : Transfo.local =
  apply_on_path (simpl_proj_aux)


(* [Struct_modif]: a module for defining struct modifications *)
module Struct_modif = struct
  (* Fields of a struct *)
  type fields = (label * typ) list

  let fields_identity :  fields -> fields =
    Fun.id

  (* [modif] is the type of a function such as [f_get],
     which is meant to be called as [f_get aux t], where
     [aux] is the function for recursively processing subterms. *)
  type recmodif = (trm->trm)

  (* Arguments for [struct_modif]:
     - [f_fields] is for modifying struct fields
     - [f_get] is for [get(access(base,f))]
     - [f_set] is for [set(access(base, f), rhs)]
     - [f_struct_get] is for [struct_get(base, f)]
     - [f_access] is for [struct_access(base, f)]
     - [f_alloc] is for [trm_struct ls]; the function is provided with the
       old list and the new list of fields, with names and types. *)
  type arg = {
    f_fields : fields -> fields;
    f_get: recmodif -> trm -> trm;
    f_set: recmodif -> trm -> trm;
    f_struct_get: recmodif -> trm -> trm;
    f_access: recmodif -> trm -> trm;
    f_alloc: (fields*fields) -> recmodif -> trm -> trm;
  }

  let arg_must_not_happen : recmodif -> trm -> trm =
    (fun _ _ -> assert false)

  let arg_identity : recmodif -> trm -> trm =
    (fun _aux t -> t)

  let reuse_annot_of (tsrc : trm) (t : trm) : trm =
    { t with  annot = tsrc.annot; marks = tsrc.marks }

end

(* [modif_accesses struct_name arg t]: modify struct accesses
    params:
      [old_and_new_fields]: used to replace some specific fields
      [struct_name]: used for checking the type of the struct access
      [arg]: see Struct_modif module
      [t]: trm corresponding to the surrounding sequence of the targeted typedef *)
let modif_accesses (old_and_new_fields : Struct_modif.fields * Struct_modif.fields) (struct_name : var) (arg : Struct_modif.arg) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    let default () = trm_map aux t in
    let is_target_typ base =
      is_typ_struct struct_name base.typ in

    match set_struct_access_inv t with
    | Some (_field, base, _rhs) -> (* LATER: use when clause? *)
            if is_target_typ base
              then arg.f_set aux t
              else default()
    | None ->
      begin match get_struct_access_inv t with
      | Some (_field, base) ->
        if is_target_typ base
          then arg.f_get aux t
          else default()
      | None ->
        begin match struct_access_inv t with
        | Some (_field, base) ->
          if is_target_typ base
            then arg.f_access aux t
            else default()
        | None ->
          begin match struct_get_inv t with
          | Some (_field, base) ->
            if is_typ_struct struct_name base.typ
              then arg.f_struct_get aux t
              else default()
          | None ->
            begin match struct_init_inv t with
            | Some sl ->
              if is_typ_struct struct_name t.typ
                then arg.f_alloc old_and_new_fields aux t
                else default()
            | None -> default ()
            end
          end
        end
      end
    in aux t

(* [struct_modif_aux new_fields f_get f_set use_annot_of index t]:
    params:
      [arg]: Struct_modif type
      [index]: index of the typedef on its surrounding sequence
      [t]: ast of the main sequence containing the typedef definition *)
let struct_modif_aux (arg : Struct_modif.arg) (index : int)  (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let tdef = begin match Mlist.nth_opt tl index with
    | Some t1 -> t1
    | None -> assert false end in
    begin match tdef.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with
      | Typdef_prod (t_names, old_fields) ->
         let struct_name = td.typdef_tconstr in
         let new_fields = arg.f_fields old_fields in
         let new_typdef = {td with typdef_body = Typdef_prod (t_names, new_fields)} in
         let new_td = trm_typedef ~marks:tdef.marks new_typdef in
         let f_update = fun t -> new_td in
         let f_update_further = fun t -> modif_accesses (old_fields, new_fields) struct_name arg t in
         let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
         {t with desc = Trm_seq new_tl}
      | _ -> fail tdef.loc "Struct_core.Struct_core.struct_modif: expected a struct definition"
      end
    | _ -> fail tdef.loc "Struct_core.Struct_core.struct_modif: expected a target to a typedef struct definition"
    end
  | _ -> fail t.loc "Struct_core.Struct_core.struct_modif: exepcted the surrounding sequence of the typedef "

let struct_modif (arg : Struct_modif.arg) (index : int) : Transfo.local =
  apply_on_path (struct_modif_aux arg index)
