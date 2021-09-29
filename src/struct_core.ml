open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [set_explicit_aux field_list t]: transform an assigment into a list of field assignments
    params:
      t: ast of the assignment
    return:
      updated ast with the transformed assignment
 *)
let set_explicit_aux (t: trm) : trm =
  let typid_to_typedef_map = Clang_to_ast.(!ctx_typedef) in
  match t.desc with 
  | Trm_apps(_, [lt;rt]) ->
      let tid_r = Internal.get_typid_from_trm rt  in
      let tid_l = Internal.get_typid_from_trm lt  in
      let tid = match tid_r, tid_l with
      | -1, _ -> tid_l
      | _, -1 -> tid_r
      | _, _ -> if tid_r = tid_l then tid_r 
                  else fail t.loc "set_explicit_aux: different types in an assignment"
      in
      let struct_def = if tid <> -1 then Typ_map.find tid typid_to_typedef_map 
                        else fail t.loc "set_explicit_aux: explicit assignemnt is supported only for struct types" in
      
      let field_list = Internal.get_field_list struct_def in
      begin match rt.desc with
      | Trm_apps(f1, [rbase]) ->
        begin match lt.desc with
        | Trm_apps (f2, [lbase]) ->
          let exp_assgn = List.map (fun (sf, ty) ->
          let new_f = trm_unop (Unop_struct_field_addr sf) in
           trm_set (trm_apps ~annot:[Access] ~typ:(Some ty) new_f [trm_apps ~annot:[Mutable_var_get] f2 [lbase]]) (trm_apps ~annot:[Access] ~typ:(Some ty) new_f [trm_apps ~annot:[Mutable_var_get] f1 [rbase]])
          ) field_list in
         trm_seq_no_brace exp_assgn 
        | _ -> let exp_assgn = List.map(fun (sf, ty) ->
          let new_f = trm_unop (Unop_struct_field_addr sf) in
          trm_set (trm_apps ~annot:[Mutable_var_get] ~typ:(Some ty) new_f [lt]) (trm_apps ~annot:[Access] ~typ:(Some ty) f1 [trm_apps ~annot:[Mutable_var_get] new_f [rbase]])
          ) field_list in
          trm_seq_no_brace exp_assgn 
        end
      (* If the right hand side is a struct initialization *)
      | Trm_struct st ->
        let st = Mlist.to_list st in
        begin match lt.desc with
        | Trm_apps (f2, lbase) ->
          let exp_assgn = List.mapi(fun i (sf, ty) ->
          let new_f = trm_unop (Unop_struct_field_addr sf) in
          trm_set (trm_apps ~annot:[Access] ~typ:(Some ty) new_f [trm_apps ~annot:[Mutable_var_get] f2 lbase]) (List.nth st i)
          ) field_list in
          trm_seq_no_brace exp_assgn 
        | Trm_var v ->
          let exp_assgn = List.mapi(fun i (sf, ty) ->
          let new_f = trm_unop (Unop_struct_field_addr sf) in
          trm_set (trm_apps ~typ:(Some ty) new_f [trm_var v]) (List.nth st i)
          ) field_list in
          trm_seq_no_brace exp_assgn 
        | _ -> fail t.loc "set_explicit_aux: left term was not matched"
        end
      | _ -> let exp_assgn = List.map (fun (sf, ty) ->
              let new_f = trm_unop (Unop_struct_field_addr sf) in
                trm_set (trm_apps ~annot:[Access] ~typ:(Some ty) new_f [lt]) (trm_apps ~annot:[Access] ~typ:(Some ty) new_f [rt])
                ) field_list in
             trm_seq_no_brace exp_assgn 
      end
    
    | _ -> fail t.loc "set_explicit_aux: this expression is not supported"
   
let set_explicit : Target.Transfo.local =
  Target.apply_on_path(set_explicit_aux )


(* [set_implicit t] transform a sequence with a list of explicit assignments into
      a single assignment.
    pararms:
      t: ast of the sequence containing the assignments
    return:
      updated ast with the transfored assignments
 *)
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
               | Trm_val (Val_prim (Prim_unop (Unop_struct_field_addr _)))
               | Trm_val (Val_prim (Prim_unop (Unop_struct_field_get _)))->
                  [rt]

               | _ -> fail f'.loc "set_implicit_aux: expected a struct acces on the right hand side of the assignment"
               end
              | _ -> fail f'.loc "set_implicit_aux: expected a trm_apps"
            end
            | Trm_val (Val_prim (Prim_unop (Unop_struct_field_addr _)))
            | Trm_val (Val_prim (Prim_unop (Unop_struct_field_get _)))->
                  [rt]
            | _ -> fail f'.loc "set_implicit_aux: expected a struct acces on the right hand side of the assignment"
           end
          | _ -> acc @ [rhs]
          end
      | _ -> fail t.loc "set_implicit_aux: expected a set operation"
    ) [] tl in
    let first_instruction = Mlist.nth tl 0 in
    begin match first_instruction.desc with
    | Trm_apps(f,[lhs;_]) ->
          begin match f.desc with
          | Trm_val ( Val_prim ( Prim_binop Binop_set) ) ->
            let lt = begin match lhs.desc with
            | Trm_apps(f', [lt]) ->
              begin match f'.desc with
              | Trm_val (Val_prim (Prim_unop (Unop_struct_field_addr _)))
              | Trm_val (Val_prim (Prim_unop (Unop_struct_field_get _)))-> lt
              | _ -> fail f'.loc "set_implicit_aux: expected a struct access on the left hand side of the assignment"
              end
            | _ -> fail lhs.loc "set_implicit_aux: expected a struct access"
            end
            in
            begin match rhs_trms with
            | [rhs1] -> trm_set lt rhs1
            | _ -> trm_set lt (trm_struct (Mlist.of_list rhs_trms))
            end
          | _ -> fail f.loc "set_explicit_aux: expected an assignment instruction"
          end
      | _ -> fail t.loc "set_implicit_aux: expected a sequence with all explicit assignments"

    end
  | _ -> fail t.loc "set_implicit_aux: sequence which contains the set instructions was not matched"

let set_implicit (keep_label : bool) : Target.Transfo.local =
  Target.apply_on_path (Internal.apply_on_path_targeting_a_sequence ~keep_label (set_implicit_aux) "set_implicit")


(* [inline_struct_accesses x t]: change all the occurrences of the struct accesses to a field into a field
    params:
      x: the name of the field for which the transformation is applied
      t: ast node located in the same level as the stract declaration or deeper
    return:
      updated ast node with the transformed field accesses
*)
let inline_struct_accesses  (x : var) (t : trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, [base]) ->
      begin match f.desc with
      | Trm_val (Val_prim (Prim_unop (Unop_struct_field_addr y)))
        | Trm_val (Val_prim (Prim_unop (Unop_struct_field_get y))) ->
          (* Removed this if else condition just for debugging purposes *)
          (* if false then fail t.loc ("Accessing field " ^ x ^ " is impossible, this field has been deleted during inlining")
          else  *)
          begin match base.desc with
          | Trm_apps (f',base') ->
            begin match f'.desc with
            | Trm_val(Val_prim (Prim_binop Binop_array_cell_addr))
              | Trm_val(Val_prim (Prim_binop Binop_array_cell_get)) ->
                (* THen base caontains another base and also the index  *)
                let base2 = List.nth base' 0 in
                let index = List.nth base' 1 in
                begin match base2.desc with
                | Trm_apps(f'',base3) ->
                  begin match f''.desc with
                  | Trm_val (Val_prim (Prim_unop Unop_struct_field_addr z))
                    | Trm_val (Val_prim (Prim_unop (Unop_struct_field_get z ))) when z = x ->
                    let new_var = z ^ "_" ^ y in
                    let new_f = {f' with desc = Trm_val(Val_prim (Prim_unop (Unop_struct_field_addr new_var)))} in
                    trm_apps ~annot:t.annot  f' [trm_apps new_f base3;index]
                  | _ -> trm_map (aux global_trm) t
                  end
                | _ -> fail t.loc "inline_struct_accesses: expected a trm_apps"
                end
            | Trm_val (Val_prim (Prim_unop (Unop_struct_field_addr z)))
              | Trm_val (Val_prim (Prim_unop (Unop_struct_field_get z))) when z = x ->
                let new_var = z ^"_"^ y in
                let new_f = {f' with desc = Trm_val(Val_prim (Prim_unop (Unop_struct_field_addr new_var)))}
              in
              trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
                     ~add:t.add ~typ:t.typ new_f base'
            | _ -> trm_map (aux global_trm) t
            end
          | _ -> trm_map (aux global_trm) t
          end
      | _ -> trm_map (aux global_trm) t
      end
    | _ -> trm_map (aux global_trm) t
in
aux t t
(* [inline_struct_initialization struct_name field_list field_index t]: change all struct in struct initializations
    params:
      struct_name: the type of the struct which is being inlined
      field_list: a list of fields from the original type of the struct
      field_index: index of the field in the outer struct
      t: ast node located in the same level as the main struct declaration or deeper
    return:
      updated ast nodes with the changed struct in struct initializations
*)
let inline_struct_initialization (struct_name : string) (field_list : field list) (field_index : int) (t : trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    match t.desc with
    | Trm_struct term_list ->
      begin match t.typ with
      | Some { typ_desc = Typ_constr (y, _, _); _} when y = struct_name ->
        let trm_to_change = Mlist.nth term_list field_index in
        begin match trm_to_change.desc with
        | Trm_struct _ ->  trm_struct (Internal.inline_sublist_at field_index term_list)(* trm_struct (Tools.insert_sublist_in_list term_list_to_inline field_index term_list) *)
        | Trm_apps(_, [base]) ->
          begin match base.desc with
          | Trm_var p ->
            let trm_list_to_inline = List.map(fun x ->
              trm_apps ~annot: [Access] (trm_unop (Unop_get))[
                trm_apps (trm_unop (Unop_struct_field_addr x)) [
                  trm_var p
                ]
              ]
            ) (List.rev field_list)
            in
            let term_list = Mlist.remove field_index field_index term_list in
            let new_term_list = Mlist.insert_sublist_at field_index trm_list_to_inline term_list in
            trm_struct new_term_list
          | _ -> fail base.loc "inline_struct_initialization: expected a heap allocated variable"
          end
        | _ -> trm_map (aux global_trm) t
        end
      | _ -> trm_map (aux global_trm) t
      end
    | _ -> trm_map (aux global_trm) t
  in
  aux t t

(* [inline_aux field_to_inline index t]: replace [field_to_inline] with a list of fields coming from
      the fields of the type of [field_to_inlne] which should be a typedef struct. Then it will change all
       the accesses of this field to accesses of the field.
    params:
      field_to_inline: field which is going to be inlined
      index: index of the struct declaration inside the sequence it belongs
      t: ast of the struct declaration
    return:
      update ast with the inline struct in struct and changed all struct accesses
*)
let inline_aux (field_to_inline : field) (index : int) (t : trm ) =
  match t.desc with
  | Trm_seq tl ->
    let lfront, td, lback =  Internal.get_trm_and_its_relatives index tl in
    let typid_to_typedef_map = Clang_to_ast.(!ctx_typedef) in
    begin match td.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with
      | Typdef_prod (t_names, field_list) ->
       let field_index = Internal.get_field_index field_to_inline (List.rev field_list) in
       let lfront1, lback1 = Tools.split_list_at field_index (List.rev field_list) in
       let field_to_inline1, lback1 = if List.length lback1 = 1 then (lback1, []) else
        Tools.split_list_at 1 lback1 in
       let _ ,field_type = List.nth field_to_inline1 0 in
       let tyid = begin match field_type.typ_desc with
       | Typ_constr (_, tid , _) -> tid
       | Typ_array (ty1, _) ->
        begin match ty1.typ_desc with
        | Typ_constr (_, tid, _) -> tid
        | _ -> fail t.loc "inline_aux: expected a typ_constr"
        end
       | _ -> fail t.loc  "inline_aux: expected a typ_constr"
       end
       in
       let struct_def = Typ_map.find tyid typid_to_typedef_map in
       let inner_type_field_list = begin match struct_def.typdef_body with
        | Typdef_prod (_, s) -> s
        | _ -> fail t.loc "inline_aux: the field wanted to inline should have also a struct typedef"
        end
       in
       let inner_type_field_list = List.map (fun (x, typ) ->
            match field_type.typ_desc with
            | Typ_array (_, size) -> (field_to_inline ^ "_" ^ x, (typ_array typ size))
            | _ -> (field_to_inline ^ "_" ^ x, typ)) inner_type_field_list in

       let field_list = List.rev  (lfront1 @ (List.rev inner_type_field_list) @ lback1) in
       let new_typedef = {td with typdef_body =  Typdef_prod (t_names, field_list)} in
       let new_trm = trm_typedef new_typedef in
       let lback = Mlist.map (inline_struct_accesses field_to_inline) lback in
       let lback = Mlist.map (inline_struct_initialization td.typdef_tconstr (List.rev (fst (List.split (Internal.get_field_list struct_def)))) field_index) lback in
       let new_tl = Mlist.merge lfront lback in
       let new_tl = Mlist.insert_at index new_trm new_tl in
       trm_seq ~annot:t.annot ~marks:t.marks  new_tl
      | _ -> fail t.loc "inline_aux: expected a struct "
      end
    | _ -> fail t.loc "inline_aux: expected a trm_typedef"
    end
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"

let inline (field_to_inline : field) (index : int) : Target.Transfo.local =
  Target.apply_on_path (inline_aux field_to_inline index)



(* [fields_reorder_aux struct_fields move_where around t]: reorder fields of a struct
    params:
      struct_fields: a list of fields to move
      move_where: a string which is equal either to before or after
      around: the target field where fields are going to move
      t: ast of the typedef struct
    return:
      updated ast of the typedef struct declaration
 *)

let fields_reorder_aux (struct_fields: var list) (move_where : reorder) (t: trm) : trm =
  match t.desc with
  | Trm_typedef td ->
   begin match td.typdef_body with
   | Typdef_prod (tn, fs) ->
    let field_list = Internal.reorder_fields move_where struct_fields fs in
   trm_typedef {td with typdef_body = Typdef_prod (tn, field_list)}
  | _ -> fail t.loc "fields_reorder_aux: expected a typdef_prod"
  end
  | _ -> fail t.loc "fields_reorder_aux: expected a typedef definiton"

(* [fields_reorder struct_fields move_where around t p] *)
let fields_reorder (struct_fields : var list) (move_where : reorder) : Target.Transfo.local =
  Target.apply_on_path(fields_reorder_aux struct_fields move_where)

(* [inline_struct_accesses name field t] transform a specifi struct access into a variable
      occurrence.
    params:
      name: name of the variable to replace teh struct access
      field: struct accesses on this field are going to be replaced with name
      t: ast node located in the same level as the variable declaration
    return:
      updated ast node with all the struct accesses changed to variable occurrences
*)
let inline_struct_accesses (name : var) (field : var) (t : trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    begin match t.desc with
    | Trm_apps (f, [base]) ->
      begin match f.desc with
      | Trm_val (Val_prim (Prim_unop (Unop_struct_field_addr y)))
        | Trm_val (Val_prim (Prim_unop (Unop_struct_field_get y))) when y = field ->
          begin match base.desc with
          | Trm_var v when v = name ->
            trm_var (name ^ "_" ^ field)
          | _ -> trm_map (aux global_trm) t
          end
      | _ -> trm_map (aux global_trm) t
      end
    | _ -> trm_map (aux global_trm) t
    end
   in aux t t


(* [to_variables_aux index t] change a variable declaration of type typedef struct into a list
      of variable declarations with types inherited from the fields of the underlying type.
    params:
      index: index of the declaration inside the sequence it belongs to.
      t: ast of the surrounding sequence of the variable declarations
    return:
      updated surrounding sequence
*)
let to_variables_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, trm_to_change, lback = Internal.get_trm_and_its_relatives index tl in
    begin match trm_to_change.desc with
    | Trm_let (vk, (x, tx), init) ->
      let typid_to_typedef_map = Clang_to_ast.(!ctx_typedef) in
      let typid = begin match vk with
                  | Var_immutable ->
                    begin match tx.typ_desc with
                    | Typ_constr (_, tid, _) -> tid
                    | _ -> fail t.loc "struct_to_variables_aux: expected a struct type"
                    end
                  | Var_mutable -> begin match (get_inner_ptr_type tx).typ_desc with
                                   | Typ_constr (_, tid, _) -> tid
                                   | _ -> fail t.loc "struct_to_variables_aux: expected a struct type"
                                   end
                  end in
      let struct_def = Typ_map.find typid typid_to_typedef_map in
      let field_list = Internal.get_field_list struct_def in
      let struct_init_list = begin match init.desc with
                             | Trm_apps(_, [base]) ->
                              begin match base.desc with
                              | Trm_struct ls -> (Mlist.to_list ls)
                              | _ -> fail init.loc "struct_to_variables_aux: expected a struct initialisation"
                              end
                             | Trm_struct ls -> (Mlist.to_list ls)
                             | _ -> []
                             end in
      let var_decls = List.mapi( fun  i (sf, ty) ->
          match struct_init_list with
          | [] -> trm_let Var_mutable (x ^ "_" ^sf, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut ty) (trm_prim (Prim_new ty))
          | _ -> trm_let Var_mutable (x ^ "_" ^sf, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut ty) (trm_apps (trm_prim (Prim_new ty)) [List.nth struct_init_list i])
      ) field_list in
      let lback = Mlist.map (fun t1 ->
        List.fold_left (fun t2 f1 ->
          inline_struct_accesses x f1 t2
        ) t1 (List.rev (fst (List.split field_list)))
      ) lback in
      let new_tl = Mlist.merge lfront lback in
      let new_tl = Mlist.insert_sublist_at index var_decls new_tl in
      trm_seq ~annot:t.annot ~marks:t.marks new_tl 

   | _ -> fail trm_to_change.loc "struct_to_variables_aux: expected a variable declaration"
    end
  | _ -> fail t.loc "struct_to_variables_aux: expected the surrounding sequence"


let to_variables (index : int) : Target.Transfo.local =
  Target.apply_on_path (to_variables_aux index)