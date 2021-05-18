open Ast 
open Ast_to_c
open Transformations
open Tools 

let rec tile_array_core (base_type : typ) (block_name : typvar) (b : trm) (x : typvar)
  (t : trm) : trm =
  (*
    replace sizeof(base_type) with sizeof(block_name)
    if another term is used for size: use b * t_size
   *)
  let new_size (t_size : trm) : trm =
    if Ast_to_c.ast_to_string t_size =
         "sizeof(" ^ Ast_to_c.typ_to_string base_type ^ ")"
    then trm_var ("sizeof(" ^ block_name ^ ")")
    else trm_apps (trm_binop Binop_mul) [b; t_size]
  in
  let new_alloc (t_alloc : trm) : trm =
    match t_alloc.desc with
    (* expectation: my_alloc(nb_elements, size_element) *)
    | Trm_apps (t_alloc_fun, [t_nb_elts; t_size_elt]) ->
       (* goal: my_alloc(nb_elements / b, b * size_element) *)
       let t_nb_elts = trm_apps (trm_binop Binop_div) [t_nb_elts; b] in
       let t_size_elt = new_size t_size_elt in
       trm_apps t_alloc_fun [t_nb_elts; t_size_elt]
    (* there's possibly a cast first *)
    | Trm_apps (t_cast,
                [{desc = Trm_apps (t_alloc_fun,
                                   [t_nb_elts; t_size_elt]); _}]) ->
       let t_nb_elts = trm_apps (trm_binop Binop_div) [t_nb_elts; b] in
       let t_size_elt = new_size t_size_elt in
       trm_apps t_cast [trm_apps t_alloc_fun [t_nb_elts; t_size_elt]]
    | _ -> fail t.loc "new_alloc: expected array allocation"
  in
  match t.desc with
  (* declarations *)
  | Trm_decl d ->
     begin match d with
     (* we have to change the declaration of x *)
     | Def_typ (y, ty) when y = x ->
        (* ty must be an array type or a pointer type *)
        begin match ty.ty_desc with
        | Typ_ptr ty ->
           (* ty* becomes (ty[b])* *)
           trm_seq ~annot:(Some No_braces)
              [
                trm_decl (Def_typ (block_name, typ_array ty (Trm b)));
                trm_decl (Def_typ (y, typ_ptr (typ_var block_name)))
              ]
        | Typ_array (ty, s) ->
           (* ty[s] becomes ty[s/b][b] *)
           begin match s with
           | Undefined -> fail t.loc "tile_array_core: array size must be provided"
           | Const n ->
              let n_div_b =
                trm_apps (trm_binop Binop_div) [trm_lit (Lit_int n); b]
              in
              trm_seq ~annot:(Some No_braces)
                [
                  trm_decl (Def_typ (block_name, typ_array ty (Trm b)));
                  trm_decl (Def_typ (y, typ_array (typ_var block_name)
                                          (Trm n_div_b)))
                ]
           | Trm t' ->
              let t'' = trm_apps (trm_binop Binop_div) [t'; b] in
              trm_seq ~annot:(Some No_braces)
                [
                  trm_decl (Def_typ (block_name, typ_array ty (Trm b)));
                  trm_decl (Def_typ (y, typ_array (typ_var block_name)
                                          (Trm t'')))
                ]
           end
        | _ -> fail t.loc "tile_array_core: expected array or pointer type declaration"
        end
     (*
       other cases: type declarations (not x), fun declarations, var
       declarations (not of type x)
       arrays of type x are heap allocated
      *)
     | _ -> trm_map (tile_array_core base_type block_name b x) t
     end
  (* heap allocations *)
  | Trm_seq [t_decl;
             {desc = Trm_apps
                       ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                        [t_var; t_alloc]); _}]
       when t.annot = Some Heap_allocated ->
     let ty = var_decl_type t_decl in
     begin match ty.ty_desc with
     (* we look for arrays of type x *)
     | Typ_ptr {ty_desc = Typ_var y; _} when y = x ->
        trm_seq ~annot:(Some Heap_allocated)
          [t_decl;
           trm_apps ~annot:(Some Initialisation_instruction)
             (trm_binop Binop_set) [t_var; new_alloc t_alloc]
          ]
     | _ -> trm_map (tile_array_core base_type block_name b x) t
     end
  (* set with alloc *)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
              [lhs; rhs]) ->
     (* lhs should have type x *)
     begin match lhs.typ with
     | Some {ty_desc = Typ_var y; _} when y = x ->
        trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add
          ~typ:t.typ (trm_binop Binop_set) [lhs; new_alloc rhs]
     | _ -> trm_map (tile_array_core base_type block_name b x) t
     end
  (* array accesses *)
  | Trm_apps (f, tl) ->
     begin match f.desc with
     | Trm_val (Val_prim (Prim_binop Binop_array_access))
       | Trm_val (Val_prim (Prim_binop Binop_array_get)) ->
        begin match tl with
        | [base; index] ->
           begin match base.typ with
           (* we only look for arrays of type x *)
           | Some {ty_desc = Typ_var y; _} when y = x ->
              (* replace base[index] with base[index/b][index%b] *)
              trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add
                ~typ:t.typ f
                [
                  trm_apps ~annot:base.annot ~loc:base.loc ~is_statement:false
                    ~add:base.add ~typ:base.typ f
                    [
                      {base with typ = match base.typ with
                                         | None -> None
                                         | Some ty -> Some (typ_ptr ty)
                      };
                      trm_apps (trm_binop Binop_div) [index; b]
                    ];
                  trm_apps (trm_binop Binop_mod) [index; b]
                ]
           | _ -> trm_map (tile_array_core base_type block_name b x) t
           end
        | _ -> fail t.loc "tile_array_core: array accesses must have two arguments"
        end
     | _ -> trm_map (tile_array_core base_type block_name b x) t
     end
  | _ -> trm_map (tile_array_core base_type block_name b x) t

let array_to_variables_core (clog : out_channel) (new_vars : var list) (decl_trm : trm) (decl_index : int) (t  : trm) : trm =
  let log : string =
    let loc : string =
    match t.loc with
    | None -> ""
    | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in Printf.sprintf
    (" - expression\n%s\n" ^^
    " %s is a declaration\n"
    )
    (ast_to_string decl_trm) loc
    in write_log clog log;
    match t.desc with
    | Trm_seq tl ->
       let decl_type = begin match decl_trm.desc with
       | Trm_seq[t_decl] ->
        begin match t_decl.desc with
        | Trm_decl (Def_var ((_,_),dx)) ->
          begin match dx.desc with
          | Trm_val( Val_prim (Prim_new t_arr)) ->
            begin match t_arr.ty_desc with
            | Typ_array (t_var,_) ->
              begin match t_var.ty_desc with
              | Typ_var x -> x
              | _ -> fail t.loc "array_to_variables_core: expected a type variable"
              end
            | _ -> fail t.loc "array_to_variables_core: expected an array type"
            end
          | _ -> fail t.loc "array_to_variables_core: something went wrong"
          end
        | _ -> fail t.loc "array_to_variables_core: expected a variable declaration"
        end
      | _ -> fail t.loc "array_to_variables_core: expected a sequence which contain the variable declaration"
      end
      in
      (* let decl_index = get_index decl_trm tl in *)
      let new_trms = List.map(fun x ->
        trm_seq ~annot:(Some Heap_allocated) [trm_decl (Def_var((x,typ_ptr (typ_var decl_type)),trm_prim (Prim_new (typ_var decl_type))))]) new_vars
      in
      trm_seq ~annot:t.annot (insert_sublist_in_list new_trms decl_index tl)
    | _ -> fail t.loc "array_to_variables_core: only declaration inside sequence are supported"