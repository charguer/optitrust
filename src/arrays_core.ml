open Ast 
open Clang_to_ast
open Target
open Generic

(* This is an auxiliary function for array to variables to modify the ast globally *)
let inline_array_access (array_var : var) (new_vars : var list) (t: trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    match t.desc with
    | Trm_apps(f,[arr_base;arr_index]) ->
      begin match f.desc with
      | Trm_val (Val_prim (Prim_binop Binop_array_access)) ->
        begin match arr_base.desc with
        | Trm_var x when x = array_var ->
          begin match arr_index.desc with
          | Trm_val (Val_lit (Lit_int i)) ->
            if i >= List.length new_vars then fail t.loc "inline_array_access: not enough new_variables entered"
            else
              trm_var (List.nth new_vars i)
          | _ -> fail t.loc "inline_array_access: only integer indexes are allowed"
          end
        | Trm_apps (f1,[base1]) ->
          begin match f1.desc with
          | Trm_val (Val_prim (Prim_unop Unop_struct_access var)) when var = array_var ->
            begin match arr_index.desc with
            | Trm_val (Val_lit (Lit_int i)) ->
              if i >= List.length new_vars then fail t.loc "inline_array_access: not enough new_variables entered"
              else
                let f1 = {f1 with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access (List.nth new_vars i))))} in
                trm_apps f1 [base1]
                (* trm_var (List.nth new_vars i) *)
            | _ -> fail t.loc "inline_array_access: only integer indexes are allowed"
            end
          | _ -> trm_map (aux global_trm) t
          end
        | _ -> trm_map (aux global_trm) t
        end
      | _ -> trm_map (aux global_trm) t
      end
    | _ -> trm_map (aux global_trm) t
  in aux t t

(* array_to_variables_aux: This is an auxiliary function for array_to_variables 
    params:
      new_vars: a list of strings of length equal to the size of the array
      subt: an ast subterm
    return
      the updated ast
*)
let array_to_variables_aux  (new_vars : var list) (subt  : trm) : trm =
  match subt.desc with 
  | Trm_let (_,(_, _), init) ->
    begin match init.desc with
    | Trm_val( Val_prim (Prim_new t_arr)) ->
      begin match t_arr.ty_desc with
      | Typ_array (t_var,_) ->
        begin match t_var.ty_desc with
        | Typ_var (y, _) -> 
          let new_trms = List.map(fun x ->
          trm_let Var_mutable (x,(typ_ptr (typ_var y (get_typedef y)))) (trm_lit (Lit_uninitialized))) new_vars
          in
          trm_seq ~annot:subt.annot new_trms 

        | _ -> fail subt.loc "array_to_variables_core: expected a type variable"
        end
      | _ -> fail subt.loc "array_to_variables_core: expected an array type"
      end
    | _ -> fail subt.loc "array_to_variables_core: something went wrong"
    end
  | _ -> fail subt.loc "array_to_variables_core: expected a variable declaration"


(* array_to_variables: Transofrm an array declaration into multiple variables declarations
    params:
      new_vars: a list of strings of length equal ot the size of the array
      path_to_decl: path to the array declaration
      t: ast
    return:
      the updated ast
 *)
let array_to_variables (new_vars : var list) (path_to_decl : path) (t : trm) : trm =
  apply_local_transformation (array_to_variables_aux new_vars) t path_to_decl


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
  | Trm_typedef d ->
     begin match d with
     (* we have to change the declaration of x *)
     | Typedef_abbrev (y, ty) when y = x ->
        (* ty must be an array type or a pointer type *)
        begin match ty.ty_desc with
        | Typ_ptr ty ->
           (* ty* becomes (ty[b])* *)
           trm_seq ~annot:(Some No_braces)
              [
                trm_typedef (Typedef_abbrev(block_name, typ_array ty (Trm b)));
                trm_typedef (Typedef_abbrev(y, typ_ptr (typ_var block_name (get_typedef block_name))))
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
                  trm_typedef (Typedef_abbrev(block_name, typ_array ty (Trm b)));
                  trm_typedef (Typedef_abbrev(y, typ_array (typ_var block_name (get_typedef block_name))
                                          (Trm n_div_b)))
                ]
           | Trm t' ->
              let t'' = trm_apps (trm_binop Binop_div) [t'; b] in
              trm_seq ~annot:(Some No_braces)
                [
                  trm_typedef (Typedef_abbrev(block_name, typ_array ty (Trm b)));
                  trm_typedef (Typedef_abbrev(y, typ_array (typ_var block_name (get_typedef block_name))
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
  | Trm_let (Var_mutable, (y,ty), init) when y = x ->
    begin match ty.ty_desc with
    | Typ_ptr {ty_desc = Typ_var (y,_); _} when y = x ->
        trm_let Var_mutable (y,ty) init
    | _ -> trm_map (tile_array_core base_type block_name b x) t
    end
  (* set with alloc *)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
              [lhs; rhs]) ->
     (* lhs should have type x *)
     begin match lhs.typ with
     | Some {ty_desc = Typ_var (y, _); _} when y = x ->
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
           | Some {ty_desc = Typ_var (y, _); _} when y = x ->
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

(* array_swap_aux: This is an auxiliary function for array_swap
    params:
      x: typvar
      t: global ast
    return: 
      the updated ast
 *)
 let rec array_swap_aux (x : typvar) (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, tl) ->
     begin match f.desc with
     (* array accesses… *)
     | Trm_val (Val_prim (Prim_binop Binop_array_access))
       | Trm_val (Val_prim (Prim_binop Binop_array_get)) ->
        begin match tl with
        | [base; index] ->
           begin match base.desc with
           | Trm_apps (f', tl') ->
              begin match f'.desc with
              (* we look for two successive accesses to an array of type x *)
              | Trm_val (Val_prim (Prim_binop Binop_array_access))
                | Trm_val (Val_prim (Prim_binop Binop_array_get)) ->
                 begin match tl' with
                 | [base'; index'] ->
                    begin match base'.typ with
                    (* if we find such accesses, we swap the two indices *)
                    | Some {ty_desc = Typ_var (x', _); _} when x' = x ->
                       (* x might also be the type of arrays in indices… *)
                       let swapped_index = array_swap_aux x index in
                       let swapped_index' = array_swap_aux x index' in
                       trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
                         ~typ:t.typ f
                         [
                           trm_apps ~annot:base.annot ~loc:base.loc
                             ~is_statement:base.is_statement ~typ:base.typ f'
                             [
                               base';
                               swapped_index
                             ];
                           swapped_index'
                         ]
                    (*
                      otherwise we recursively call array_swap_aux after removing
                      one dimension
                     *)
                    | _ ->
                       let swapped_l = List.map (array_swap_aux x) tl in
                       trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
                         ~typ:t.typ f swapped_l
                    end
                 | _ ->
                    fail f'.loc ("swap_coordinates: array accesses should " ^
                                   "have 2 arguments");
                 end
              (*
                again, if we do not find two successive accesses, we
                recursively call array_swap_aux
               *)
              | _ ->
                 let swapped_l = List.map (array_swap_aux x) tl in
                 trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement f
                   ~typ:t.typ swapped_l
              end
           (* again, … *)
           | _ ->
              let swapped_l = List.map (array_swap_aux x) tl in
              trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement f
                ~typ:t.typ swapped_l
           end
        | _ -> fail f.loc ("swap_coordinates: array accesses should have 2 " ^
                             "arguments");
        end
     (*
         for most other terms we only recursively call array_swap_aux
         note: arrays of type x might appear in f now
      *)
     | _ ->
        let swapped_f = array_swap_aux x f in
        let swapped_l = List.map (array_swap_aux x) tl in
        trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~typ:t.typ
          swapped_f swapped_l
     end
  (* declaration… *)
  | Trm_typedef d ->
     begin match d with
     (* we look for the declaration of x *)
     | Typedef_abbrev (y, ty) when y = x ->
        (*
          swap the two first coordinates of the multidimensional array type ty
         *)
        let rec swap_type (ty : typ) : typ =
          match ty.ty_desc with
          | Typ_array ({ty_desc = Typ_array (ty', s'); ty_annot; ty_attributes},
                       s) ->
             begin match ty'.ty_desc with
             (* we look for the 2 first coordinates… *)
             | Typ_array _ ->
                let t' =
                  swap_type {ty_desc = Typ_array (ty', s'); ty_annot;
                             ty_attributes}
                in
                {ty_desc = Typ_array (t', s); ty_annot = ty.ty_annot;
                 ty_attributes = ty.ty_attributes}
             (* once we reach them, we swap them *)
             | _ ->
                {ty_desc = Typ_array ({ty_desc = Typ_array (ty', s);
                                       ty_annot = ty.ty_annot;
                                       ty_attributes = ty.ty_attributes}, s');
                 ty_annot; ty_attributes}
             end
          | _ -> fail None ("swap_type: must be an array")
        in
        trm_typedef ~annot: t.annot ~loc: t.loc ~is_statement:t.is_statement ~add:t.add
          (Typedef_abbrev (y, swap_type ty))
     (*
         all the interesting cases are covered now, we only have to do recursive
         calls
         var and fun decl first
      *)
     | _ -> trm_map (array_swap_aux x) t
     end
  (*
     remaining cases: val, var, array, struct, if, seq, while, for, switch,
     abort, labelled
     inside values, array accesses may only happen in array sizes in types
     todo: currently ignored, is it reasonable to expect such things to happen?
   *)
  | _ -> trm_map (array_swap_aux x) t