open Prelude
open Target


(* [set_explicit_aux t]: transforms an assigment into a list of field assignments,
     [t] - ast of the assignment. *)
let set_explicit_aux (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, [lt; rt], _) ->
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
                  else trm_fail t "Record_core.set_explicit_aux: different types in an assignment"
      in
      let struct_def =
        if tid <> -1 then
          match Context.typid_to_typedef tid with
          | Some td -> td
          | _ -> trm_fail t "Record_core.set_explicit_aux: could not get the declaration of typedef"
        else begin
          Tools.debug "%s" (AstC_to_c.ast_to_string t);
          trm_fail t "Record_core.set_explicit_aux: explicit assignment cannot operate on unknown types"
        end
      in
      let field_list = Internal.get_field_list t struct_def in
      begin match rt.desc with
      | Trm_apps (f1, [rt1], _) when is_get_operation rt ->
         let exp_assgn = List.mapi (fun i (sf, ty) ->
          trm_set (trm_struct_access ~typ:ty lt sf) {rt with desc = Trm_apps (f1, [trm_struct_access ~typ:ty rt1 sf], []); typ = Some ty}
         ) field_list in
         trm_seq_nobrace_nomarks exp_assgn
      | Trm_record st ->
        let st = Xlist.split_pairs_snd (Mlist.to_list st) in
        let exp_assgn = List.mapi (fun i (sf, ty) ->
          trm_set (trm_struct_access ~typ:ty lt sf) (List.nth st i)
        ) field_list
         in
        trm_seq_nobrace_nomarks exp_assgn
      | _ ->  (* other cases are included here *)
        let exp_assgn = List.mapi (fun i (sf, ty) ->
         trm_set (trm_struct_access ~typ:ty lt sf) (trm_struct_get ~typ:ty rt sf)
         ) field_list in
         trm_seq_nobrace_nomarks exp_assgn
      end
  | _ -> trm_fail t "Record_core.set_explicit_aux: expected a set operation"

(* [set_explicit t p]: applies [set_explicit_aux] at trm [t] with path [p]. *)
let set_explicit : Transfo.local =
  apply_on_path(set_explicit_aux )


(* [set_implicit t]: transform a sequence with a list of explicit field assignments into a single assignment,
      [t] - ast of the sequence containing the assignments. *)
let set_implicit_aux (t: trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     let rhs_trms = Mlist.fold_left ( fun acc instr ->
      match instr.desc with
      | Trm_apps (_, [_;rhs], _) ->
        begin match rhs.desc with
        | Trm_apps(f', [rt], _)  ->
          begin match f'.desc with
          | Trm_val ( Val_prim ( Prim_unop Unop_get ) ) ->
            begin match rt.desc with
              | Trm_apps(f'',[rt], _) ->
               begin match f''.desc with
               | Trm_val (Val_prim (Prim_unop (Unop_struct_access _)))
               | Trm_val (Val_prim (Prim_unop (Unop_struct_get _)))->
                  [trm_get rt]
               | _ -> trm_fail f' "Record_core.set_implicit_aux: expected a struct acces on the right hand side of the assignment"
               end
              | _ -> trm_fail f' "Record_core.set_implicit_aux: expected a trm_apps"
            end
            | Trm_val (Val_prim (Prim_unop (Unop_struct_access _)))
            | Trm_val (Val_prim (Prim_unop (Unop_struct_get _)))->
                  [rt]
            | _ -> trm_fail f' "Record_core.set_implicit_aux: expected a struct acces on the right hand side of the assignment"
           end
          | _ -> acc @ [rhs]
          end
      | _ -> trm_fail t "Record_core.set_implicit_aux: expected a set operation"
    ) [] tl in
    let first_instruction = Mlist.nth tl 0 in
    begin match first_instruction.desc with
    | Trm_apps(f,[lhs;_], _) ->
          begin match f.desc with
          | Trm_val ( Val_prim ( Prim_binop Binop_set) ) ->
            let lt = begin match lhs.desc with
            | Trm_apps(f', [lt], _) ->
              begin match f'.desc with
              | Trm_val (Val_prim (Prim_unop (Unop_struct_access _)))
              | Trm_val (Val_prim (Prim_unop (Unop_struct_get _)))-> lt
              | _ -> trm_fail f' "Record_core.set_implicit_aux: expected a struct access on the left hand side of the assignment"
              end
            | _ -> trm_fail lhs "Record_core.set_implicit_aux: expected a struct access"
            end
            in
            begin match rhs_trms with
            | [rhs1] -> trm_pass_labels t (trm_set lt rhs1)
            | _ ->
              let rhs_trms = List.map (fun t1 -> (None, t1)) rhs_trms in
              trm_pass_labels t (trm_set lt (trm_record (Mlist.of_list rhs_trms)))
            end
          | _ -> trm_fail f "Record_core.set_explicit_aux: expected an assignment instruction"
          end
      | _ -> trm_fail t "Record_core.set_implicit_aux: expected a sequence with all explicit assignments"

    end
  | _ -> trm_fail t "Record_core.set_implicit_aux: sequence which contains the set instructions was not matched"

(* [set_implicit keep_label t p]: applies [set_implicit_aux] at trm [t] with path [p]. *)
let set_implicit (keep_label : bool) : Transfo.local =
  apply_on_path (set_implicit_aux)

(* [inline_struct_accesses x t]: changes all the occurrences of the struct accesses to a field into a field,
      [x] - the name of the field for which the transformation is applied,
      [t] - ast node located in the same level as the stract declaration or deeper. *)
let inline_struct_accesses (x : field) (t : trm) : trm =
  let rec aux (outer_field : string) (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, base, _) ->
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
        | _ -> trm_fail f "Record_core.inline_struct_access: suspicious struct access"
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
        | _ -> trm_fail f "Record_core.inline_struct_access: suspicious struct access"
        end
      | _ -> trm_map (aux outer_field) t
      end
    | _ -> trm_map (aux outer_field) t

   in aux "" t

(* [inline_struct_initialization struct_name field_list field_index t]: changes all struct in struct initializations,
      [struct_name] - the type of the struct that is being inlined,
      [field_list] - a list of fields from the original type of the struct,
      [field_index] - index of the field in the outer struct,
      [t] - ast node located in the same level as the main struct declaration or deeper. *)
let inline_struct_initialization (struct_name : string) (field_list : field list) (field_index : int) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    (* Searching for struct intialization lists of type typedef struct {} struct_name *)
    | Trm_record term_list ->
      begin match t.typ with
      | Some ty ->
        let ty = get_inner_const_type ty in
        begin match ty.typ_desc with
        | Typ_constr (y, _, _) when (typconstr_has_name y struct_name) ->
          let lfront, (_,trm_to_change) , lback = Mlist.get_item_and_its_relatives field_index term_list in
          begin match trm_to_change.desc with
          | Trm_record sl ->
            let new_term_list = Mlist.merge_list [lfront; sl; lback] in
            trm_record ~annot:t.annot ?typ:t.typ new_term_list

          | Trm_apps (_, [{desc = Trm_var p;_} as v], _) when is_get_operation trm_to_change ->
            let sl = List.map (fun f -> (None, trm_get (trm_struct_access (trm_var ?typ:v.typ p) f))) field_list in
            let new_term_list = Mlist.merge_list [lfront; Mlist.of_list sl; lback] in
            trm_record ~annot:t.annot ?typ:t.typ new_term_list

          | Trm_var p ->
            let sl = List.map (fun f -> (None, trm_struct_get (trm_var ?typ:t.typ p) f)) field_list in
            let new_term_list = Mlist.merge_list [lfront; Mlist.of_list sl; lback] in
            trm_record ~annot:t.annot ?typ:t.typ new_term_list

          | _ -> trm_fail t "Record_core.inline_struct_initialization: struct intialization list is not compatible with definition"
          end
        | _ -> trm_map aux t
        end
      | _ -> trm_fail t "Record_core.inline_struct_initialization: couldn't find the type of the struct intitialization type, try reparsing first"
        end
    | _ -> trm_map aux t
  in aux t

(* [reveal_field_aux field_to_reveal index t]: reveals field [field_to_reveal] on its typedef struct definition,
     update all the initializations and accesses according to this change,
      [field_to_reveal] - field that is going to be revealed,
      [index] - index of the struct declaration inside the sequence it belongs to,
      [t] - trm corresponding to a typedef struct definition. *)
let reveal_field_aux (field_to_reveal : field) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let td_name = ref "" in
    let f_list = ref [] in
    let field_index = ref 0 in
    let f_update (t : trm) : trm =
      match t.desc with
      | Trm_typedef td ->
        begin match td.typdef_body with
        | Typdef_record field_list ->
          field_index := Internal.get_field_index t field_to_reveal field_list;
          let (field_to_reveal_rf,_),field_list  = Xlist.extract_element field_list !field_index in
          let field_type = get_member_type t field_to_reveal_rf in
          let tyid = begin match field_type.typ_desc with
          | Typ_constr (_, tid, _) -> tid
          | Typ_array (ty1, _) ->
            begin match ty1.typ_desc with
            | Typ_constr (_, tid, _) -> tid
            | _ -> trm_fail t "Record_core.reveal_field_aux: expected a type constr"
            end
          | _ -> trm_fail t "Record_core.reveal_field_aux: expected a typ_constr"
          end in
          let struct_def =
            if tyid <> -1
              then match Context.typid_to_typedef tyid with
                | Some td -> td
                | _ -> trm_fail t "Record_core.reveal_field_aux: could not get the declaration of typedef"
              else
                trm_fail t "Record_core.reveal_field_aux: field revealing is supported only for struct type"
            in

          let inner_type_field_list = match struct_def.typdef_body with
          | Typdef_record rfl -> rfl
          | _ -> trm_fail t "Record_core.reveal_field_aux: the field wanted to inline should also be of struct type"
          in

          let inner_type_field_list = Internal.rename_record_fields (fun f ->
             Convention.name_app field_to_reveal f) inner_type_field_list in

          let typ_update (ty : typ) : typ =
            match field_type.typ_desc with
              | Typ_array (_, size) -> typ_array ty ?size
              | _ -> ty
            in
          let inner_type_field_list = Internal.update_record_fields_type typ_update inner_type_field_list in

          let field_list = Xlist.insert_sublist_at !field_index inner_type_field_list field_list in

          td_name := td.typdef_tconstr;
          f_list := fst (List.split (Internal.get_field_list t struct_def));

          let new_typedef = {td with typdef_body = Typdef_record field_list} in
          trm_replace (Trm_typedef new_typedef) t

        | _ -> trm_fail t "Record_core.reveal_field_aux: expected a struct definition"
        end
      | _ -> trm_fail t "Record_core.reveal_field_aux: expected a target to a type definition"
      in
    let f_update_further (t : trm) : trm =
      let t = inline_struct_accesses field_to_reveal t in
      inline_struct_initialization !td_name !f_list !field_index t
      in
    let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> trm_fail t "Record_core.reveal_field_aux: expected the surrounding sequence"

(* [reveal_field field_to_reveal index t p]: applies [reveal_field] at trm [t] with path [p]. *)
let reveal_field (field_to_reveal : field) (index : int) : Transfo.local =
  apply_on_path (reveal_field_aux field_to_reveal index)


(* [compute_bijection order fl]: based on the [order] given, computes the bijection
    of the indices after applying that order. *)
let compute_bijection (order : fields_order) (fl : (field * int) list) : int list =
  match order with
  | Move_before (field, fields_to_move) ->
    let filtered_fl = List.filter (fun (f, _) -> not (List.mem f fields_to_move)) fl in
    let fields_to_move_ind = List.map (fun f -> match List.assoc_opt f fl with
      | Some ind -> (f, ind)
      | None -> failwith "Record_core.compute_bijection: catastrophic error ."
    ) fields_to_move in
    let upd_fl =
    List.fold_left (fun acc (f, ind) ->
      if f = field then fields_to_move_ind @ (f, ind) :: acc
        else (f, ind) :: acc
    ) [] (List.rev filtered_fl)
      in
    List.map snd upd_fl
  | Move_after (field, fields_to_move) ->
    let filtered_fl = List.filter (fun (f, _) -> not (List.mem f fields_to_move)) fl in
    let fields_to_move_ind = List.map (fun f -> match List.assoc_opt f fl with
      | Some ind -> (f, ind)
      | None -> failwith "Record_core.compute_bijection: catastrophic error ."
    ) fields_to_move in
    let upd_fl =
    List.fold_left (fun acc (f, ind) ->
      if f = field then (f, ind) :: fields_to_move_ind @acc
        else (f, ind) :: acc
    ) [] (List.rev filtered_fl)
      in
    List.map snd upd_fl
  | Reorder_all order ->
    if List.length order <> List.length fl then failwith "Record_core.compute_bijection: Reorder all should contain all the fields.";
    List.map (fun f -> match List.assoc_opt f fl with
      | Some ind -> ind
      | None -> failwith "Record_core:compute_bijection: couldn't find field %s." f
    ) order

(* [reorder_fields_aux order index t]: reorders the fields of the struct [t] based on [order],
     [order] - order based on which the fields will be reordered,
     [t] - ast of the typedef Record. *)
let reorder_fields_aux (order : fields_order) (index : int) (t : trm) : trm =
  let error = "Record_core.reorder_fields_aux: expected the surrouding sequence of the targeted declaration." in
  let tl = trm_inv ~error trm_seq_inv t in
  let bij = ref [] in
  let struct_name = ref "" in
  let f_update (t : trm) : trm =
    match t.desc with
    | Trm_typedef td ->
      struct_name := td.typdef_tconstr;
      begin match td.typdef_body with
      | Typdef_record rfl ->
        let rfl_str_rep = List.mapi (fun i (rf, _) ->
          match rf with
          | Record_field_member (lb, _) -> (lb, i)
          | Record_field_method t1 ->
            begin match trm_typedef_inv t1 with
            | Some td -> (td.typdef_tconstr, i)
            | _ -> trm_fail t "Record_core.reorder_fields_aux: unkown method definition."
            end
        ) rfl in
        bij := compute_bijection order rfl_str_rep;
        let new_rfl = Xlist.reorder !bij rfl in
        trm_alter ~desc:(Trm_typedef {td with typdef_body = Typdef_record new_rfl}) t

      | _ -> trm_fail t "Record_core.reorder_fields_aux: expected a target to a record type definition."

      end
    | _ -> trm_fail t "Record_core.reorder_fields_aux: expected a target pointing to a typedef."
    in
  let f_update_further (t : trm) : trm =
    let rec aux (t : trm) : trm =
      match t.desc with
      | Trm_record mlt ->
        begin match t.typ with
        | Some {typ_desc = Typ_constr (qty, _, _)} when typconstr_has_name qty !struct_name ->
          let lt = Mlist.to_list mlt in
          let reordered_lt = Xlist.reorder !bij lt in
          trm_alter ~desc:(Trm_record (Mlist.of_list reordered_lt)) t
        | _ -> trm_map aux t
        end
      | _ -> trm_map aux t
      in
    aux t
   in
  let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
  trm_replace (Trm_seq new_tl) t

(* [reorder_fields index order t p]: applies [reorder_fields_aux] at trm [t] with path [p]. *)
let reorder_fields (order : fields_order) (index : int) : Transfo.local =
  apply_on_path (reorder_fields_aux order index)

(* [inline_struct_accesses name field t]: transforms a specific struct access into a variable occurrence,
    [name] - name of the variable to replace the struct access,
    [field] - struct accesses on this field are going to be replaced with [name],
    [t] - ast node located in the same level as the variable declaration. *)
let inline_struct_accesses (name : var) (field : field) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    begin match t.desc with
    | Trm_apps (f, [base], _) ->
      begin match f.desc with
      | Trm_val (Val_prim (Prim_unop (Unop_struct_access y)))
        | Trm_val (Val_prim (Prim_unop (Unop_struct_get y))) when y = field ->
          begin match base.desc with
          | Trm_var v when v = name ->
            (* FIXME: #var-id *)
            trm_var (new_var (Convention.name_app name.name field))
          | _ -> trm_map aux t
          end
      | _ -> trm_map aux t
      end
    | _ -> trm_map aux t
    end
   in aux t

(* [to_variables_aux index t]: changes a variable declaration of type typedef struct into a list
      of variable declarations with types inherited from the fields of the underlying type,
      [index] - index of the declaration inside the sequence it belongs to,
      [t] - ast of the surrounding sequence of the variable declarations. *)
let to_variables_aux (index : int) (t : trm) : trm =
  let error = "Record_core.struct_to_variables_aux: expected the surrounding sequence." in
  let tl = trm_inv ~error trm_seq_inv t in
  let field_list = ref [] in
  let var_name = ref dummy_var in
  let f_update (t : trm) : trm =
    let error = "Record_core.struct_to_variables_aux: expected a variable declaration." in
    let (x, tx, init) = trm_inv ~error trm_let_inv t in
      var_name := x;
      let typid = begin match (get_inner_ptr_type tx).typ_desc with
        | Typ_constr (_, tid, _) -> tid
        | _ -> trm_fail t "Record_core.struct_to_variables_aux: expected a struct type"
        end in
      let struct_def =
      if typid <> -1
        then match Context.typid_to_typedef typid with
          | Some td -> td
          | _ -> trm_fail t "Record_core.to_variables_aux: could not get the declaration of typedef"
        else
          trm_fail t "Record_core.to_variables_aux: explicit assignment is supported only for struct types"
       in
      field_list := Internal.get_field_list t struct_def;
      let struct_init_list = begin match init.desc with
                           | Trm_apps(_, [base], _) ->
                            begin match base.desc with
                            | Trm_record ls -> Xlist.split_pairs_snd (Mlist.to_list ls)
                            | _ -> trm_fail init "Record_core.struct_to_variables_aux: expected a struct initialisation"
                            end
                           | Trm_record ls -> Xlist.split_pairs_snd (Mlist.to_list ls)
                           | _ -> []
                           end in
      let var_decls = List.mapi(fun i (sf, ty) ->
        (* FIXME: #var-id *)
        let new_name = new_var (Convention.name_app x.name sf) in
        match struct_init_list with
        | [] -> trm_let_mut (new_name, ty) (trm_uninitialized ())
        | _ -> trm_let_mut (new_name, ty) (List.nth struct_init_list i)

        ) !field_list in
        trm_seq_nobrace_nomarks var_decls
     in
  let f_update_further (t : trm) : trm =
    List.fold_left (fun t2 f1 ->
          inline_struct_accesses !var_name f1 t2
        ) t (fst (List.split !field_list))
    in
  let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
  trm_seq ~annot:t.annot new_tl

(* [to_variables index t p]: applies [to_variables_aux] at trm [t] with path [p]. *)
let to_variables (index : int) : Transfo.local =
  apply_on_path (to_variables_aux index)

(* TODO: merge with Variable.Rename *)
(* [Rename]: a module used for renaming the struct fields. *)
module Rename = struct
  type t = string -> string
  let add_prefix (s : string) : t =
    fun str -> s ^ str

  let only_for (pattern : string) : t -> t =
    fun tr s ->
      if Tools.pattern_matches pattern s then tr s else s
end

(* [rename]: instantiation of module [Rename]. *)
type rename = Rename.t

(* [rename_struct_accesses struct_name renam t]: renames all struct accesses based on [rename],
      [struct_name] - the constructed type whose fields are going to be renamed,
      [rename] - a type used to rename the struct fields,
      [t] - any node in the same level as the struct declaration.*)
let rename_struct_accesses (struct_name : string) (rename : rename) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, [base], _) ->
      begin match f.desc with
      | Trm_val (Val_prim (Prim_unop (Unop_struct_access y))) ->
          begin match base.typ with
          | Some ty ->
            begin match ty.typ_desc with
            | Typ_constr (x, _, _) when (typconstr_has_name x struct_name) ->
              trm_apps ~annot:t.annot ?typ:t.typ ({f with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access (rename y))))})  [base]
            | _ -> trm_map aux t
            end
          | None -> trm_map aux  t
          end
      | Trm_val (Val_prim (Prim_unop (Unop_struct_get y))) ->
        begin match base.typ with
          | Some ty ->
            begin match ty.typ_desc with
            | Typ_constr (x, _, _) when (typconstr_has_name x struct_name) ->
              trm_apps ~annot:t.annot ?typ:t.typ {f with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get (rename y))))}  [base]
            | _ -> trm_map aux t
            end
          | None -> trm_map aux t
          end

      | _ -> trm_map aux t
      end
    | Trm_apps ({desc = Trm_var qf; _} as f, args, ghost_args) when trm_has_cstyle Method_call t ->
        let member_base = fst (Xlist.uncons args) in
        begin match (get_operation_arg member_base).typ with
        | Some ty ->
          begin match ty.typ_desc with
          | Typ_constr (x, _, _) when (typconstr_has_name x struct_name) ->
            let renamed_var = { qualifier = qf.qualifier; name = rename qf.name; id = qf.id } in
            trm_apps ~annot:t.annot ?typ:t.typ ~ghost_args {f with desc = Trm_var renamed_var} args
          | _ -> trm_map aux t
          end

        | None -> trm_map aux t
        end

    | _ -> trm_map aux t
   in aux t

(* [rename_fields_aux index rename t]: renames struct fields in the typedef struct definitions,
      [index] - the index of the struct declaration in the sequence [t],
      [rename] - a type used to rename the fields,
      [t] - the ast of the sequence which contains the struct declaration. *)
let rename_fields_aux (index : int) (rename : rename) (t : trm) : trm =
  let error = "Record_core.rename_fields_aux: expected the sequence which contains the typedef declaration." in
  let tl = trm_inv ~error trm_seq_inv t in
  let struct_name = ref "" in
  let f_update (t : trm) : trm =
    match t.desc with
    | Trm_typedef ({typdef_tconstr = name; typdef_body = Typdef_record rfl;_}  as td) ->
      struct_name := name;
      let rfl = Internal.rename_record_fields rename rfl in
      trm_typedef ~annot:t.annot {td with typdef_body = Typdef_record rfl}
   | _ -> trm_fail t "Record_core.reanme_fields_aux: expected a typedef declaration"
   in
  let f_update_further (t : trm) : trm =
    rename_struct_accesses !struct_name rename t
   in
  let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
  trm_seq ~annot:t.annot new_tl

(* [rename_fields index rename t p]: applies [rename_aux] at trm [t] with path [p]. *)
let rename_fields (index : int) (rename : rename) : Transfo.local =
  apply_on_path (rename_fields_aux index rename)

(* [update_fields_type_aux pattern ty t]: changes the current type for all the struct fields,
      that are matched with [pattern] and whose type can be changed by [typ_update].
      [pattern] - regular expression to match struct fields,
      [typ_update] - function that modifies only specific types,
      [t] - the ast of the typedef definition. *)
let update_fields_type_aux (pattern : string ) (typ_update : typ -> typ) (t : trm) : trm =
  match t.desc with
  | Trm_typedef ({typdef_body = Typdef_record rfl;_}  as td) ->
    (* LATER: FIX ME! *)
    (* let update_type ty = typ_map typ_update ty in *)

    let rec update_type (ty_to_update : typ) : typ =
      match ty_to_update.typ_desc with
      | Typ_array _ | Typ_ptr _
        | Typ_const _ -> typ_map update_type ty_to_update
      | _ -> ty_to_update
      in

    (* let replace_type (s : string) (ty1 : typ) : typ =
      if Tools.pattern_matches pattern s then (update_type ty1)  else ty1 in
     *)
    let rfl = Internal.update_record_fields_type ~pattern update_type rfl in
    (* let new_fl = List.map (fun (x, ty2) -> (x, replace_type x ty2)) fl in *)
    trm_typedef ~annot:t.annot {td with typdef_body = Typdef_record rfl}
  | _ -> trm_fail t "Record_core.reanme_fields_aux: expected a typedef declaration"

(* [update_fields_type pattern typ_update t p]: applies [update_fields_type_aux] at trm [t] with path [p]. *)
let update_fields_type (pattern : string) (typ_update : typ -> typ) : Transfo.local =
  apply_on_path (update_fields_type_aux pattern typ_update )


(* [simpl_proj_aux t]: transforms all expression of the form {1, 2, 3}.f into the trm it projects to,
      [t] - ast of the node whose descendants can contain struct initialization list projections. *)
let simpl_proj_aux (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, [struct_list], _) ->
      begin match trm_prim_inv f with
      | Some (Prim_unop (Unop_struct_get x)) | Some (Prim_unop (Unop_struct_access x))->
        begin match struct_list.desc with
        | Trm_record tl ->
          let tid = Internal.get_typid_from_trm struct_list in
            if tid <> -1 then begin
              let struct_def = match Context.typid_to_typedef tid with
              | Some td -> td
              | _ -> trm_fail struct_list "Record_core.simpl_proj_aux: couldn't retrieve the the struct declaration" in
              let field_list = Internal.get_field_list struct_list struct_def in
              let field_vars = fst (List.split field_list) in
              match Xlist.index_of x field_vars  with
              | Some i -> snd (Mlist.nth tl i)
              | _ -> t
              end
            else
              t
        | _ -> trm_map aux t
        end
      | _ -> trm_map aux t
      end
    | _ -> trm_map aux t
   in aux t

(* [simpl_proj t p]: applies [simpl_proj_aux] at trm [t] with path [p]. *)
let simpl_proj : Transfo.local =
  apply_on_path (simpl_proj_aux)


(* [Struct_modif]: a module for defining struct modifications. *)
module Struct_modif = struct
  (* Fields of a struct *)

  type fields = record_fields
  (* type fields = (label * typ) list *)

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
     - [f_alloc] is for [trm_record ls]; the function is provided with the
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
    { t with  annot = tsrc.annot}

end

(* [modif_accesses struct_name arg t]: modify struct accesses,
    [old_and_new_fields] - used to replace some specific fields,
    [struct_name] - used for checking the type of the struct access,
    [arg] - see Struct_modif module,
    [t] - trm corresponding to the surrounding sequence of the targeted typedef. *)
let modif_accesses (old_and_new_fields : Struct_modif.fields * Struct_modif.fields) (struct_name : string) (arg : Struct_modif.arg) (t : trm) : trm =
  let struct_typconstr = [], struct_name in
  let is_target_typ base = is_typ_struct struct_typconstr base.typ in
  let rec aux (t : trm) : trm =
    let default () = trm_map aux t in

    match set_struct_access_inv t with
    | Some (base, _field, _rhs) -> (* LATER: use when clause? *)
            if is_target_typ base
              then arg.f_set aux t
              else default()
    | None ->
      begin match get_struct_access_inv t with
      | Some (base, _field) ->
        if is_target_typ base
          then arg.f_get aux t
          else default()
      | None ->
        begin match struct_access_inv t with
        | Some (base, _field) ->
          if is_target_typ base
            then arg.f_access aux t
            else default()
        | None ->
          begin match struct_get_inv t with
          | Some (base, _field) ->
            if is_target_typ base
              then arg.f_struct_get aux t
              else default()
          | None ->
            begin match struct_init_inv t with
            | Some sl ->
              if is_target_typ t
                then arg.f_alloc old_and_new_fields aux t
                else default()
            | None -> default ()
            end
          end
        end
      end
    in aux t

(* [struct_modif_aux new_fields f_get f_set use_annot_of index t],
     [arg] - Struct_modif type,
     [index] - index of the typedef on its surrounding sequence,
     [t] - ast of the main sequence containing the typedef definition. *)
let struct_modif_aux (arg : Struct_modif.arg) (index : int)  (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let tdef = begin match Mlist.nth_opt tl index with
    | Some t1 -> t1
    | None -> assert false end in
    begin match tdef.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with
      | Typdef_record old_fields ->
         let struct_name = td.typdef_tconstr in
         let new_fields = arg.f_fields old_fields in
         let new_typdef = {td with typdef_body = Typdef_record new_fields} in
         let new_td = trm_typedef ~annot:tdef.annot new_typdef in
         let f_update = fun t -> new_td in
         let f_update_further = fun t -> modif_accesses (old_fields, new_fields) struct_name arg t in
         let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
         trm_replace (Trm_seq new_tl) t
      | _ -> trm_fail tdef "Record_core.Record_core.struct_modif: expected a struct definition"
      end
    | _ -> trm_fail tdef "Record_core.Record_core.struct_modif: expected a target to a typedef struct definition"
    end
  | _ -> trm_fail t "Record_core.Record_core.struct_modif: exepcted the surrounding sequence of the typedef "

(* [struct_modif arg index t p]: applies [struct_modif_aux] at trm [t] with path [p]. *)
let struct_modif (arg : Struct_modif.arg) (index : int) : Transfo.local =
  apply_on_path (struct_modif_aux arg index)

(* [change_field_access_kind_aux acc_kind f t]: changes the access_kind for field [f] to [acc_kind] of class or struct [t]. *)
let change_field_access_kind_aux (acc_kind : record_field_annot) (f : field) (t : trm) : trm =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_record rfs ->
      let new_rfs = List.map (fun (rf, rf_ann) ->
        (* if f is given as the default string then all the fields access kind will be changed. *)
        if f = ""
          then (rf, acc_kind)
          else
            let rf_name_opt = Internal.get_field_name rf in
            begin match rf_name_opt with
            | Some n when n = f -> (rf, acc_kind)
            | _ -> (rf, rf_ann)
            end
        ) rfs in
        let new_td = {td with typdef_body = Typdef_record new_rfs} in
        trm_alter ~desc:(Trm_typedef new_td) t
    | _ -> trm_fail t "Record_core.change_field_access_kind_aux: expected a target to a structured typedef."
    end
  | _ -> trm_fail t "Record_core.change_field_access_kind_aux: expected a targetd to a typedef."


(* [change_field_access_kind acc_kind f t p]: applies [change_field_access_kind] at trm [t] with path [p]. *)
let change_field_access_kind (acc_kind : record_field_annot) (f : field) : Transfo.local =
  apply_on_path (change_field_access_kind_aux acc_kind f)

(* [method_to_const_aux method_name t]: converts the [method_name] method to a a const one,
    if the targeted method is already const than this transformation does nothing.
    [method_name] - the name of the method that's going to be converted.*)
let method_to_const_aux (method_name : var) (t : trm) : trm =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_record rfl ->
        let upd_rfl = List.map (fun (rf, rf_ann) ->
            match rf with
            | Record_field_method t1  ->
              if is_class_constructor t1
                then (rf, rf_ann)
                else if method_name = dummy_var then (Record_field_method (trm_add_cstyle Const_method t1), rf_ann)
                else
                  failwith "unimplemented, #var-id"
                  (*
                  begin match decl_name t1 with
                  | Some td when method_name = td.typdef_tconstr ->
                    if trm_has_cstyle Const_method t1
                      then (rf, rf_ann)(* begin Printf.printf ("Nothing to change, method %s is already const." method_name); (rf, rf_ann) end *)
                      else
                        let t1 = trm_add_cstyle Const_method t1 in
                        (Record_field_method t1, rf_ann)
                  | _ -> (rf, rf_ann)
                  end *)
            | _ -> (rf, rf_ann)
        ) rfl in

        trm_replace (Trm_typedef {td with typdef_body = Typdef_record upd_rfl}) t
    | _ ->  trm_fail t "Record_core.method_to_const_aux: expected a target to a typedef record definition."
    end
  | _ -> trm_fail t "Record_core.method_to_const_aux: expected a target to a record definition."


(* [method_to_const method_name t p]: applies [method_to_const_aux] at trm [t] with path [p]. *)
let method_to_const (method_name : var) : Transfo.local =
  apply_on_path (method_to_const_aux method_name)
