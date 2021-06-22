open Ast

(* [inline_array_access array_var new_vars t]: Change all the occurence of the array to variables
    params:
      array_var: array_variable  to apply changes on
      new_vars: a list of variables, the variables at index i replaces and occurence of array_var[i]
      t: ast subterm
*)
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



(* [to_variables_aux new_vars t]: This is an auxiliary function for to_variables
    params:
      new_vars: a list of strings of length equal to the size of the array
      t: an ast subterm
    return
      the updated ast
*)
let to_variables_aux (new_vars : var list) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let d,lback = Tools.split_list_at 1 lback in
    let d = List.hd d in
    let array_name = decl_name d in
    let var_decls = begin match d.desc with 
    | Trm_let (_, (_ , __), init) -> 
      begin match init.desc with
      | Trm_val(Val_prim (Prim_new t_arr)) ->
        begin match t_arr.typ_desc with
      | Typ_array (t_var,_) ->
        begin match t_var.typ_desc with
        | Typ_constr (y, _, _) ->
          List.map(fun x ->
          trm_let Var_mutable (x,(typ_ptr (typ_var y))) (trm_lit (Lit_uninitialized))) new_vars
    
        | _ -> fail t.loc "to_variables_aux: expected a type variable"
        end
      | _ -> fail t.loc "to_variables_aux: expected an array type"
      end
      | _ -> fail t.loc "to_variables_aux: expected a new_operation"
      end
    | _ -> fail t.loc "to_variables_aux: expected a variable declaration"
    end
    in
    let lback = List.map (inline_array_access array_name new_vars) lback in
    trm_seq ~annot:t.annot ~loc:t.loc (lfront @ var_decls @ lback)
  | _ -> fail t.loc "to_variables_aux: expected the outer sequence of the targeted trm"

(* [to_variables new_vars index t p] *)
let to_variables (new_vars : var list) (index : int): Target.Transfo.local =
  Target.apply_on_path (to_variables_aux new_vars index)



(* [apply_tiling base_type block_name b x]: Change all the occurence of the array to the tiled form
    params:
      base_type: type of the array
      block_name: new name for the array
      b: the size of the tile
      x: typvar
      t: ast subterm
    assumptions:
    - if x is ty*, each array of type x is allocated through a custom function:
      x a = my_alloc(nb_elements, size_element)
    - x is not used in function definitions, but only in var declarations
    - for now: in any case, the number of elements is divisible by b
*)
let rec apply_tiling (base_type : typ) (block_name : typvar) (b : trm) (x : typvar)
  (t : trm) : trm =
  match t.desc with
  (* array accesses *)
  | Trm_apps (f, tl) ->
     begin match f.desc with
     | Trm_val (Val_prim (Prim_binop Binop_array_access))
       | Trm_val (Val_prim (Prim_binop Binop_array_get)) ->
        begin match tl with
        | [base; index] ->
           begin match base.typ with
           (* we only look for arrays of type x *)
           | Some {typ_desc = Typ_constr  (y, _, _); _} when y = x ->
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
           | _ -> trm_map (apply_tiling base_type block_name b x) t
           end

        | _ -> fail t.loc "apply_tiling: array accesses must have two arguments"
        end
     | _ -> trm_map (apply_tiling base_type block_name b x) t
     end
  | _ -> trm_map (apply_tiling base_type block_name b x) t


(* [tile_aux: name block_name b x t] *)
let tile_aux (block_name : typvar) (b : var) (x : typvar) (index: int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let d,lback = Tools.split_list_at 1 lback in
    let d = List.hd d in
    
    let base_type = 
    begin match d.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with
      | Typdef_alias typ ->
        begin match typ.typ_desc with 
        | Typ_ptr ty -> ty
        | Typ_array(ty, _) -> ty
        | _ -> fail d.loc "tile_aux: expected array or pointer type"
        end
      | _ -> fail d.loc "tile_aux: expected a typedef abbrevation"      
      end
    | _ -> fail d.loc "tile_aux: expected a trm_typedef"
    end
    in    
    (*
    replace sizeof(base_type) with sizeof(block_name)
    if another term is used for size: use b * t_size
   *)
    let new_size (t_size : trm) : trm =
      if Ast_to_c.ast_to_string t_size =
         "sizeof(" ^ Ast_to_c.typ_to_string base_type ^ ")"
      then trm_var ("sizeof(" ^ block_name ^ ")")
      else trm_apps (trm_binop Binop_mul) [trm_var b; t_size]
    in
    let new_alloc (t_alloc : trm) : trm =
      match t_alloc.desc with
      (* expectation: my_alloc(nb_elements, size_element) *)
      | Trm_apps (t_alloc_fun, [t_nb_elts; t_size_elt]) ->
         (* goal: my_alloc(nb_elements / b, b * size_element) *)
         let t_nb_elts = trm_apps (trm_binop Binop_div) [t_nb_elts; trm_var b] in
         let t_size_elt = new_size t_size_elt in
         trm_apps t_alloc_fun [t_nb_elts; t_size_elt]
      (* there's possibly a cast first *)
      | Trm_apps (t_cast,
                  [{desc = Trm_apps (t_alloc_fun,
                                     [t_nb_elts; t_size_elt]); _}]) ->
         let t_nb_elts = trm_apps (trm_binop Binop_div) [t_nb_elts; trm_var b] in
         let t_size_elt = new_size t_size_elt in
         trm_apps t_cast [trm_apps t_alloc_fun [t_nb_elts; t_size_elt]]
      | _ -> fail t.loc "new_alloc: expected array allocation"
    in
    let array_decl = begin match d.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with 
      | Typdef_alias ty when td.typdef_tconstr = x ->
         begin match ty.typ_desc with
        | Typ_ptr ty -> 
           (* ty* becomes (ty[])* *)
           trm_seq ~annot:(Some No_braces)
              [
                trm_typedef {
                  td with typdef_tconstr = block_name; 
                  typdef_body = Typdef_alias (typ_array ty (Trm (trm_var b)))};
                trm_typedef { 
                  td with typdef_tconstr = td.typdef_tconstr; 
                  typdef_body = Typdef_alias (typ_ptr (typ_constr block_name td.typdef_typid []))}]
        | Typ_array (ty, s) ->
           (* ty[s] becomes ty[s/b][b] *)
           begin match s with
           | Undefined -> fail t.loc "tile_aux: array size must be provided"
           | Const n ->
              let n_div_b =
                trm_apps (trm_binop Binop_div) [trm_lit (Lit_int n); trm_var b]
              in
              trm_seq ~annot:(Some No_braces)
                [
                  trm_typedef {
                    td with typdef_tconstr = block_name;
                    typdef_body = Typdef_alias (typ_array ty (Trm (trm_var b)))};
                  trm_typedef{
                    td with typdef_tconstr = td.typdef_tconstr;
                    typdef_body = Typdef_alias (typ_array (typ_var block_name) (Trm n_div_b))}]
           | Trm t' ->
              let t'' = trm_apps (trm_binop Binop_div) [t'; trm_var b] in
              trm_seq ~annot:(Some No_braces)
                [
                  trm_typedef {
                    td with typdef_tconstr = block_name;
                    typdef_body = Typdef_alias (typ_array ty (Trm (trm_var b)))};
                
                  trm_typedef {
                    td with typdef_tconstr = td.typdef_tconstr;
                    typdef_body = Typdef_alias (typ_array (typ_var block_name) (Trm t''))}]
           end
        | _ -> fail t.loc "tile_aux: expected array or pointer type declaration"
        end
      | _ -> fail t.loc "tile_aux: no enums expected"
      end
    | Trm_let (Var_mutable, (y,ty), init) when y = x ->
        begin match ty.typ_desc with 
        | Typ_ptr {typ_desc = Typ_constr (y, _, _); _} when y = x ->
          trm_let Var_mutable (y, ty) init
        | _ -> fail t.loc "tile_aux: expected a pointer because of heap allocation"
        end
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
              [lhs; rhs]) ->
        (* lhs should have type x *)
        begin match lhs.typ with
        | Some {typ_desc = Typ_constr (y, _, _); _} when y = x ->
           trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add
             ~typ:t.typ (trm_binop Binop_set) [lhs; new_alloc rhs]
        | _ -> trm_map (apply_tiling base_type block_name (trm_var b) x) t
        end
    | _-> fail t.loc "tile_aux: expected a declaration"
      end
     
    in
    let lback = List.map (apply_tiling base_type block_name (trm_var b) x) lback in
    trm_seq ~annot:t.annot (lfront @ [array_decl] @ lback)

  | _ -> fail t.loc "tile_aux: expected the surrounding sequence of the targeted trm"

(* [tile name block_name b x index p t] *)
let tile (block_name : typvar) (b : var) (x : typvar) (index : int): Target.Transfo.local =
  Target.apply_on_path(tile_aux block_name b x index)


(* [apply_swapping x t]: This is an auxiliary function for array_swap
    params:
      x: typvar
      t: global ast
    return:
      the updated ast
 *)
 let rec apply_swapping (x : typvar) (t : trm) : trm =
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
                    | Some {typ_desc = Typ_constr (x', _, _); _} when x' = x ->
                       (* x might also be the type of arrays in indices… *)
                       let swapped_index = apply_swapping x index in
                       let swapped_index' = apply_swapping x index' in
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
                      otherwise we recursively call apply_swapping after removing
                      one dimension
                     *)
                    | _ ->
                       let swapped_l = List.map (apply_swapping x) tl in
                       trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
                         ~typ:t.typ f swapped_l
                    end
                 | _ ->
                    fail f'.loc ("swap_coordinates: array accesses should " ^
                                   "have 2 arguments");
                 end
              (*
                again, if we do not find two successive accesses, we
                recursively call apply_swapping
               *)
              | _ ->
                 let swapped_l = List.map (apply_swapping x) tl in
                 trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement f
                   ~typ:t.typ swapped_l
              end
           (* again, … *)
           | _ ->
              let swapped_l = List.map (apply_swapping x) tl in
              trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement f
                ~typ:t.typ swapped_l
           end
        | _ -> fail f.loc ("swap_coordinates: array accesses should have 2 " ^
                             "arguments");
        end
     (*
         for most other terms we only recursively call apply_swapping
         note: arrays of type x might appear in f now
      *)
     | _ ->
        let swapped_f = apply_swapping x f in
        let swapped_l = List.map (apply_swapping x) tl in
        trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~typ:t.typ
          swapped_f swapped_l
     end

  (*
     remaining cases: val, var, array, struct, if, seq, while, for, switch,
     abort, labelled
     inside values, array accesses may only happen in array sizes in types
     todo: currently ignored, is it reasonable to expect such things to happen?
   *)
  | _ -> trm_map (apply_swapping x) t


(* [swap_aux name x t]: This is an auxiliary function for swap
    params:
      name: a function to change the name of the array
      x: typ of the array
      t: ast 
    assumption: x is not used in fun declarations
      -> to swap the first dimensions of a function argument, use swap_coordinates
      on the array on which the function is called: a new function with the
      appropriate type is generated
      function copies are named with name
    return:
      the updated ast
*)
let swap_aux (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let d,lback = Tools.split_list_at 1 lback in
    let d = List.hd d in
    
    begin match d.desc with
      | Trm_typedef td ->
        begin match td.typdef_body with 
        | Typdef_alias ty  ->
           let rec swap_type (ty : typ) : typ =
          match ty.typ_desc with
          | Typ_array ({typ_desc = Typ_array (ty', s'); typ_annot; typ_attributes},
                       s) ->
             begin match ty'.typ_desc with
             (* we look for the 2 first coordinates… *)
             | Typ_array _ ->
                let t' =
                  swap_type {typ_desc = Typ_array (ty', s'); typ_annot;
                             typ_attributes}
                in
                {typ_desc = Typ_array (t', s); typ_annot = ty.typ_annot;
                 typ_attributes = ty.typ_attributes}
             (* once we reach them, we swap them *)
             | _ ->
                {typ_desc = Typ_array ({typ_desc = Typ_array (ty', s);
                                       typ_annot = ty.typ_annot;
                                       typ_attributes = ty.typ_attributes}, s');
                 typ_annot; typ_attributes}
             end
          | _ -> fail None ("swap_type: must be an array")
        in
        let new_decl = 
        trm_typedef ~annot: t.annot ~loc: t.loc ~is_statement:t.is_statement ~add:t.add
          {td with typdef_body = Typdef_alias (swap_type ty)}
        in
        let lback = List.map (apply_swapping td.typdef_tconstr ) lback in 
        trm_seq ~annot:t.annot (lfront @ [new_decl] @ lback) 
        | _ -> fail t.loc "swap_aux: expected a declaration"
        end
      | _ -> fail t.loc "swap_aux: expected the typedef"
    end
    
  | _ -> fail t.loc "swap_aux: expected the surrounding sequence of the targeted trm"

(* [swap name x index p t] *)
let swap (index : int) : Target.Transfo.local =
  Target.apply_on_path (swap_aux index )


(* [swap_accesses x t]: This is an auxiliary function for aos_to_soa_aux
    params: 
      x: typvar
      t: global ast
    return:
     the update ast
*)
let swap_accesses (struct_name : var) (x : typvar) (sz : size) (t : trm) : trm = 
  let rec aux (global_trm : trm) (t : trm) : trm = 
    match t.desc with     
    | Trm_apps(_,[get_base]) when t.annot = Some Access ->
      begin match get_base.desc with 
      | Trm_apps (f, [base]) ->
         begin match f.desc with
         | Trm_val (Val_prim (Prim_unop (Unop_struct_access _)))
           | Trm_val (Val_prim (Prim_unop (Unop_struct_get _))) ->
            begin match base.desc with
            | Trm_apps (f', [base'; index]) ->
               begin match f'.desc with
               | Trm_val (Val_prim (Prim_binop Binop_array_access))
                 | Trm_val (Val_prim (Prim_binop Binop_array_get)) ->
                  (*
                    swap accesses only if the type of base' is x (or x* in case of
                    an access on a heap allocated variable)
                   *)
                  let base'  = match base'.desc with
                  | Trm_apps (_, [base'']) when base'.annot = Some Mutable_var_get -> base''
                  | _ -> base'
                   in
                  let index  = match index.desc with
                  | Trm_apps (_, [index']) when index.annot = Some Mutable_var_get -> index'
                  | _ -> index
                   in
                  begin match base'.typ with
                  | Some {typ_desc = Typ_constr (y, _, _); _} when y = x ->
                     (* x might appear both in index and in base' *)
                     let base' = aux global_trm base' in
                     let index = aux global_trm index in
                     (* keep outer annotations *)
                     trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
                       ~add:t.add ~typ:t.typ f' [trm_apps f [base']; index]
                  | Some {typ_desc = Typ_ptr {typ_desc = Typ_constr (y, _, _); _}; _}
                       when y = x ->
                     (* x might appear both in index and in base' *)
                     let base' = aux global_trm base' in
                     let index = aux global_trm index in
                     (* keep outer annotations *)
                     trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
                       ~add:t.add ~typ:t.typ f' [trm_apps f [base']; index]
                  | _ -> trm_map (aux global_trm) t
                  end
                
               | _ -> trm_map (aux global_trm) t
               end
            | _ -> trm_map (aux global_trm) t
            end
         | _ -> trm_map (aux global_trm) t
         end
      | _ -> trm_map (aux global_trm) t
      end
    
    (* other cases: recursive call *)
    | Trm_typedef td when td.typdef_tconstr = struct_name ->
      begin match td.typdef_body with
      | Typdef_prod (tn, s) ->
        let s = List.map( fun (x, typ) -> (x, typ_array (typ) sz)) s in
        trm_typedef {td with typdef_body = Typdef_prod (tn, s)}

      | _ -> fail t.loc "swap_accesses: expected a typedef struct"
      end
    | _ -> trm_map (aux global_trm) t
  in aux t t


(* [aos_to_soa_aux t ] : Trasnform an array of structures to a structure of arrays
    params: 
      index: the index of the array declaration inside the surrounding sequence
      t: array of structures declaration
    return: 
      the updated ast
*)
let aos_to_soa_aux (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl ->
     let lfront, lback = Tools.split_list_at index tl in 
     let d,lback = Tools.split_list_at 1 lback in
     let d = List.hd d in      
     begin match d.desc with 
     | Trm_let (vk, (n, dx), _) ->
       begin match dx.typ_desc with 
       | Typ_ptr ty ->
        begin match ty.typ_desc with 
        | Typ_array (a, size) ->
          let struct_name = 
          begin match a.typ_desc with
          | Typ_constr (sn,_, _) -> sn
          | _ -> fail d.loc "aos_to_soa_aux: expected a typ_constr"
          end
          in
          let new_decl = trm_let vk (n,typ_ptr a) (trm_prim ~loc:t.loc (Prim_new a)) in
          let lfront = List.map (swap_accesses struct_name "" size) lfront in
          let lback = List.map (swap_accesses struct_name "" size) lback in
          trm_seq ~annot:(t.annot) (lfront @ [new_decl] @ lback)
        | _ -> fail t.loc "expected an arrays of structures declaration"
        end
      
       | _ -> fail t.loc "aos_to_soa: didn't expected a const declaration"
       end
      | Trm_typedef td ->
        begin match td.typdef_body with 
        | Typdef_alias ty ->
          begin match ty.typ_desc with 
          | Typ_array (a, size)->
            let struct_name = 
            begin match a.typ_desc with 
            | Typ_constr (sn, _, _) -> sn
               
            | _ -> fail d.loc "aos_to_soa_aux: expected a typ_constr"
            end
            in
            let new_decl = trm_typedef {td with typdef_body  = Typdef_alias a} in
            let lfront = List.map (swap_accesses struct_name td.typdef_tconstr size) lfront in
            let lback = List.map (swap_accesses struct_name td.typdef_tconstr size) lback in
            trm_seq ~annot:(t.annot) (lfront @ [new_decl] @ lback)
          | _ -> fail d.loc "aos_to_soa_aux: expected a typedef of array of structures"
          end
        | _ -> fail t.loc "aos_to_soa_aux: expected a typedef_alias"
        end 
       
     | _ -> fail d.loc "aos_to_soa_aux: expected the array declaration"
     end 

  | _ -> failwith "aos_to_soa_aux: expected the surrounding sequence"


(* [aos_to_soa name x index p t] *)
let aos_to_soa (index : int) : Target.Transfo.local =
  Target.apply_on_path(aos_to_soa_aux index)