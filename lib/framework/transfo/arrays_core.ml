open Prelude

(** [inline_array_access array_var new_vars t]: change all the occurences of the array to variables,
    params:
      [array_var]: array_variable  to apply changes on
      [new_vars]: a list of variables, the variables at index i replaces and occurence of [array_var[i]]
      [t]: ast node located in the same level or deeper as the array declaration
    return:
        updated ast with the replaced array accesses to variable references. *)
let inline_array_access (array_var : var) (new_vars : vars) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps ({desc = Trm_prim (Prim_binop Binop_array_access);_}, [base; index], _) ->
      begin match base.desc with
      | Trm_apps ({desc = Trm_prim (Prim_unop Unop_get); _}, [{desc = Trm_var x}], _) when (var_eq x array_var) ->
        begin match index.desc with
        | Trm_lit (Lit_int i) ->
          if i >= List.length new_vars
            then trm_fail index "Arrays_core.inline_array_access: the number of variable provided should be consistent with the size of the targeted array"
            else (trm_var (List.nth new_vars i))
        | Trm_apps ({desc = Trm_var x; _}, _, _) when var_has_name x "ANY" ->
          let nb_vars = List.length new_vars in
          trm_address_of (trm_apps (trm_var (name_to_var "CHOOSE")) ((trm_lit (Lit_int nb_vars)) :: (List.map trm_var_get new_vars)))
        | _ -> trm_fail index "Arrays_core.inline_array_access: only integer indices are supported"
        end
      | _ ->  trm_map aux t
      end
    | _ -> trm_map aux t
   in aux t

(** [to_variables_at new_vars t]: transform an array declaration into a list of variable declarations
      the list of variables should be entered by the user. The number of variables should correspond to
      the size of the arrys. The variable at index i in [new_vars] will replace the array occurrence
      at index i.
      (new_vars] - a list of strings of length equal to the size of the array,
      [index] - index of the instruction inside the sequence,
      [t] - ast of the surrounding sequence of the array declaration. *)
let to_variables_at (new_vars : string list) (index : int) (t : trm) : trm =
  let new_vars = List.map new_var new_vars in
  match t.desc with
  | Trm_seq tl ->
    let array_var = ref dummy_var in
    let f_update_at (t : trm) : trm =
      begin match t.desc with
        | Trm_let ((x , tx), init) ->
          array_var := x;
          begin match typ_array_inv (get_inner_ptr_type tx) with
          | Some (t_var, _) ->
            trm_seq_nobrace_nomarks (
              List.map (fun x ->
              trm_let_mut ~annot:t.annot (x, t_var) (trm_uninitialized ?loc:init.loc ())) new_vars)
          | _ -> trm_fail t "Arrays_core.to_variables_aux: expected an array type"
          end
        | _ -> trm_fail t "Arrays_core.to_variables_aux: expected a variable declaration"
        end
      in

    let f_update_further (t : trm) : trm =
      inline_array_access !array_var new_vars t
      in
    let new_tl = Mlist.update_at_index_and_fix_beyond index f_update_at f_update_further tl in

    trm_seq ~annot:t.annot ?loc:t.loc new_tl

  | _ -> trm_fail t "Arrays_core.to_variables_aux: expected the outer sequence of the targeted trm"

(** [apply_tiling base_type block_name b x]: changes all the occurences of the array to the tiled form,
      [base_type] - type of the array
      [block_name] - new name for the array
      [b] - the size of the tile
      [x] - typvar
      [t] - ast node located in the same level or deeper as the array declaration

    assumptions:
    - if x is ty*, each array of type x is allocated through a custom function:
      x a = my_alloc(nb_elements, size_element)
    - x is not used in function definitions, but only in var declarations
    - for now: in any case, the number of elements is divisible by b. *)
let rec apply_tiling (base_type : typ) (block_name : typvar) (b : trm) (x : typvar) (t : trm) : trm =
  let aux = apply_tiling base_type block_name b x in
  match t.desc with
  | Trm_apps ({desc = Trm_prim (Prim_unop Unop_get)}, [arg], _) ->
    begin match arg.desc with
    | Trm_apps ({desc = Trm_prim (Prim_binop Binop_array_access)}, [base;index], _) ->
      Pattern.pattern_match base.typ [
        Pattern.(some (typ_constr (var_eq x))) (fun () ->
          get_array_access (get_array_access base (trm_div index b)) (trm_mod index b)
        );
        Pattern.__ (fun () -> trm_map aux t)
      ]
    | _ -> trm_map aux t
    end
  | _ -> trm_map aux t

(** [tile_at block_name block_size index t]: transform an array declaration from a normal shape into a tiled one,
    then call apply_tiling to change all the array occurrences into the correct form.
      [block_name] - the name of the arrays representing one tile,
      [block_size] - the size of the tile,
      [index] - the index of the instruction inside the sequence,
      [t] - ast of the outer sequence containing the array declaration.

   Ex:
    t[N] -> t[N/B][B]  where t has the targeted type
    t[i] -> t[i/B][i%B] *)
let tile_at (block_name : string) (block_size : var) (index: int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, d, lback = Mlist.get_item_and_its_relatives index tl in
    let base_type_name, base_type =
    begin match d.desc with
    | Trm_typedef td ->
      begin match td.typedef_body with
      | Typedef_alias typ ->
        Pattern.pattern_match typ [
          Pattern.(typ_ptr !__ ^| typ_array !__ __) (fun ty () -> td.typedef_name, ty);
          Pattern.__ (fun () -> trm_fail d "Arrays_core.tile_at: expected array or pointer type")
        ]
      | _ -> trm_fail d "Arrays_core.tile_at: expected a typedef abbrevation"
      end
    | _ -> trm_fail d "Arrays_core.tile_at: expected a trm_typedef"
    end
    in
    let block_name = if block_name = "" then base_type_name.name ^ "_BLOCK" else block_name in
    let block_typvar = name_to_typvar block_name in
    (* replace sizeof(base_type) with sizeof(block_name) if another term is used for size: use b * t_size *)
    let new_size (t_size : trm) : trm =
      if Ast_to_c.ast_to_string t_size =
         "sizeof(" ^ Ast_to_c.typ_to_string base_type ^ ")"
      then trm_toplevel_var ("sizeof(" ^ block_name ^ ")")
      else trm_apps (trm_binop Binop_mul) [trm_var block_size; t_size]
    in
    let new_alloc (t_alloc : trm) : trm =
      match t_alloc.desc with
      (* expectation: my_alloc(nb_elements, size_element) *)
      | Trm_apps (t_alloc_fun, [t_nb_elts; t_size_elt], _) ->
         (* goal: my_alloc(nb_elements / b, b * size_element) *)
         let t_nb_elts = trm_apps (trm_binop Binop_div) [t_nb_elts; trm_var block_size] in
         let t_size_elt = new_size t_size_elt in
         trm_apps t_alloc_fun [t_nb_elts; t_size_elt]
      (* there's possibly a cast first *)
      | Trm_apps (t_cast,
                  [{desc = Trm_apps (t_alloc_fun,
                                     [t_nb_elts; t_size_elt], _); _}], _) ->
         let t_nb_elts = trm_apps (trm_binop Binop_div) [t_nb_elts; trm_var block_size] in
         let t_size_elt = new_size t_size_elt in
         trm_apps t_cast [trm_apps t_alloc_fun [t_nb_elts; t_size_elt]]
      | _ -> trm_fail t "Arrays_core.tile_at: expected array allocation"
    in
    let array_decl = begin match d.desc with
    | Trm_typedef td ->
      begin match td.typedef_body with
      | Typedef_alias ty  ->
        Pattern.pattern_match ty [
          Pattern.(typ_ptr !__) (fun ty () ->
            (* ty* becomes (ty[])* *)
           trm_seq_nobrace_nomarks [
            trm_typedef {
              typedef_name = block_typvar;
              typedef_body = Typedef_alias (typ_array ty ~size:(trm_var block_size))};
            trm_typedef {
              typedef_name = td.typedef_name;
              typedef_body = Typedef_alias (typ_ptr (typ_var block_typvar))}]
          );
          Pattern.(typ_array !__ !__) (fun ty s () ->
            (* ty[s] becomes ty[s/b][b] *)
            match s with
            | None -> trm_fail t "Arrays_core.tile_at: array size must be provided"
            | Some t' ->
              let t'' = trm_apps (trm_binop Binop_div) [t'; trm_var block_size] in
              trm_seq_nobrace_nomarks [
                trm_typedef {
                  typedef_name = name_to_typvar block_name;
                  typedef_body = Typedef_alias (typ_array ty ~size:(trm_var block_size))};

                trm_typedef {
                  typedef_name = td.typedef_name;
                  typedef_body = Typedef_alias (typ_array (typ_var block_typvar) ~size:t'')}]
          );
          Pattern.__ (fun () -> trm_fail t "Arrays_core.tile_at: expected array or pointer type declaration")
        ]
      | _ -> trm_fail t "Arrays_core.tile_at: no enums expected"
      end

    | Trm_let ((y,ty), init) when var_has_name y base_type_name.name ->
      Pattern.pattern_match ty [
        Pattern.(typ_ptr (typ_constr !(var_eq base_type_name))) (fun yc () ->
          trm_let ~annot:d.annot (y, ty) init
        );
        Pattern.__ (fun () -> trm_fail t "Arrays_core.tile_at: expected a pointer because of heap allocation")
      ]
    | Trm_apps ({desc = Trm_prim (Prim_binop Binop_set); _}, [lhs; rhs], _) ->
      (* lhs should have type x *)
      Pattern.pattern_match lhs.typ [
        Pattern.(some (typ_constr !(var_eq base_type_name))) (fun y () ->
          trm_apps ~annot:t.annot ?loc:t.loc ?typ:t.typ (trm_binop Binop_set) [lhs; new_alloc rhs]
        );
        Pattern.__ (fun () -> trm_map (apply_tiling base_type block_typvar (trm_var block_size) base_type_name) t)
      ]
    | _-> trm_fail t "Arrays_core.tile_at: expected a declaration"
      end
    in
    let lback = Mlist.map (apply_tiling base_type block_typvar (trm_var block_size) base_type_name) lback in
    let new_tl = Mlist.merge lfront lback in
    let new_tl = Mlist.insert_at index array_decl new_tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> trm_fail t "Arrays_core.tile_at: expected the surrounding sequence of the targeted trm"


(** [apply_swapping x t]: swaps all array accesses that have type [t],
      [x] - typvar,
      [t] - an ast node which on the same level as the array declaration or deeper. *)
let rec apply_swapping (x : typvar) (t : trm) : trm =
  let aux = apply_swapping x in
  match t.desc with
  | Trm_apps ({desc = Trm_prim (Prim_unop Unop_get); _}, [arg], _) ->
    begin match arg.desc with
    | Trm_apps ({desc = Trm_prim (Prim_binop (Binop_array_access));_}, [base; index], _) ->
      begin match get_array_access_inv base with
      | Some (base1, index1) ->
        Pattern.pattern_match base1.typ [
          Pattern.(some (typ_constr (var_eq x))) (fun () ->
            get_array_access (get_array_access base1 index) index1
          );
          Pattern.__ (fun () -> trm_map aux t)
        ]
      | None -> trm_map aux t
      end
    | _ -> trm_map aux t
    end
  | Trm_apps ({desc = Trm_prim (Prim_binop (Binop_array_access));_}, [base; index], _) ->
      begin match get_array_access_inv base with
      | Some (base1, index1) ->
        Pattern.pattern_match base1.typ [
          Pattern.(some (typ_constr (var_eq x))) (fun () ->
            array_access (get_array_access base1 index) index1
          );
          Pattern.__ (fun () -> trm_map aux t)
        ]
      | None -> trm_map aux t
      end
  | _ -> trm_map aux t


(** [swap_at index t]: swap the dimensions of an array declaration,
     [index] - index of the array declaration on its surrouding sequence,
     [t] - AST of the surrouding sequence of the targeted array declaration. *)
let swap_at (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->

    let rec swap_type (ty : typ) : typ =
      Pattern.pattern_match ty [
        Pattern.(typ_array !(typ_array !__ !__) !__) (fun ty_arr_in ty' s' s () ->
          begin match typ_array_inv ty' with
          (* we look for the 2 first coordinates… *)
          | Some _ ->
            let t' = swap_type ty_arr_in in
            typ_array t' ?size:s
          (* once we reach them, we swap them *)
          | None ->
            typ_array (typ_array ty' ?size:s) ?size:s'
          end
        );
        Pattern.__ (fun () -> trm_fail t "Arrays_core.swap_type: the main target should point an an array declaration")
      ]
    in

    let f_update (t : trm) : trm =
      match t.desc with
      | Trm_typedef td ->
        begin match td.typedef_body with
        | Typedef_alias ty ->
          trm_typedef ~annot:t.annot ?loc:t.loc {td with typedef_body = Typedef_alias (swap_type ty)}
        | _ -> trm_fail t "Arrays_core.swap_aux: expected a type alias definition."
        end
      | _ -> trm_fail t "Arrays_core.swap_aux: expected the typedef instruction."

      in

    let f_update_further (t : trm) : trm =
      let td = Mlist.nth tl index in
      match td.desc with
      | Trm_typedef td -> apply_swapping td.typedef_name t
      | _ -> trm_fail t "Arrays_core.swap_aux: expected a target to a type definition"
      in
    let new_tl = Mlist.update_at_index_and_fix_beyond index f_update f_update_further tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> trm_fail t "swap_aux: expected the surrounding sequence of the targeted trm"


(** [aos_to_soa_rec struct_name sz t] : transforms an array of structures to a structure of arrays *)
(* TODO: Reimplement it from scratch *)
let aos_to_soa_rec (struct_name : typvar) (sz : var) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    (* LATER: document  E.G.  matching (array_access(struct_access(t, f), index)); ... *)
    | Trm_apps(_,[get_base], _) when is_access t  ->
      begin match get_base.desc with
      | Trm_apps (f, [base], _) ->
         begin match f.desc with
         | Trm_prim (Prim_unop (Unop_struct_access _))
           | Trm_prim (Prim_unop (Unop_struct_get _)) ->
            begin match base.desc with
            | Trm_apps (f', [base'; index], _) ->
               begin match f'.desc with
               | Trm_prim (Prim_binop Binop_array_access)
                 | Trm_prim (Prim_binop Binop_array_get) ->
                  (*
                    swap accesses only if the type of base' is x (or x* in case of
                    an access on a heap allocated variable)
                   *)

                   (*
                     LATER: Arthur write better specification for the changes
                      <<t>> [i].x
                   *)
                   (* TODO ARTHUR *)

                  let base'  = match base'.desc with
                  | Trm_apps (_, [base''], _) when is_get_operation base'  -> base''
                  | _ -> base'
                   in
                  let index  = match index.desc with
                  | Trm_apps (_, [index'], _) when is_get_operation index -> index'
                  | _ -> index
                   in
                  Pattern.pattern_match base'.typ [
                    Pattern.(some (typ_array (typ_constr (var_eq struct_name)) __)
                    ^| some (typ_ptr (typ_constr (var_eq struct_name)))
                    ^| some (typ_constr (var_eq struct_name))
                    ) (fun () ->
                      (* x might appear both in index and in base' *)
                      let base' = aux base' in
                      let index = aux index in
                      (* keep outer annotations *)
                      trm_apps ~annot:t.annot ?loc:t.loc ?typ:t.typ f' [trm_apps f [base']; index]
                    );
                    Pattern.__ (fun () -> trm_map aux t)
                  ]
               | _ -> trm_map aux t
               end
            | _ -> trm_map aux t
            end
         | _ -> trm_map aux t
         end
      | _ -> trm_map aux t
      end
      (* LATER: arthur: test the use of exceptions for structuring this code *)

    | Trm_typedef td when var_eq td.typedef_name struct_name ->
      begin match td.typedef_body with
      | Typedef_record rf ->
        let rf = List.map (fun (rf1, rf_annot) ->
          match rf1 with
          | Record_field_member (lb, ty) -> ((Record_field_member (lb, typ_array ty ~size:(trm_var sz))), rf_annot)
          | Record_field_method _ -> (Record_field_method (trm_map aux t), rf_annot)
        ) rf in
        trm_typedef ~annot:t.annot {td with typedef_body = Typedef_record rf}

      | Typedef_alias ty ->
          Pattern.pattern_match ty [
            Pattern.(typ_array !(typ_constr (var_eq struct_name)) __) (fun a () ->
              trm_typedef {td with typedef_body = Typedef_alias a}
            );
            Pattern.__ (fun () -> trm_map aux t)
          ]
      | _ -> trm_fail t "aos_to_soa_aux: expected a typedef struct"
      end
    | Trm_typedef td ->
      begin match td.typedef_body with
      | Typedef_alias ty ->
        Pattern.pattern_match ty [
          Pattern.(typ_array !(typ_constr (var_eq struct_name)) __) (fun a () ->
            trm_typedef {td with typedef_body = Typedef_alias a}
          );
          Pattern.__ (fun () -> trm_map aux t)
        ]
      | _ -> trm_map aux t
      end

    | Trm_let ((n, dx), init) ->
      Pattern.pattern_match dx [
        Pattern.(typ_ptr !(typ_array (typ_constr (var_eq struct_name)) __)) (fun a () ->
          trm_let_mut ~annot:t.annot (n, a) (trm_uninitialized ?loc:init.loc ())
        );
        Pattern.__ (fun () -> trm_map aux t)
      ]
    | _ -> trm_map aux t
  in aux t


(** [detach_init_on t]: transform an initialized array declaration into a list of write operations
      for each one of its cells
    [t] - the array declaration. *)
let detach_init_on (t : trm) : trm =
  match t.desc with
  | Trm_let ((x, tx), init) ->
    let init = match trm_ref_inv_init init with
    | Some init -> init
    | None -> trm_fail t "detach_init_on: could not get the initialization trms for the targeted array declaration" in
    begin match init.desc with
    | Trm_array tl ->
      let array_set_list =
      List.mapi ( fun i t1 ->
        trm_set (trm_apps (trm_binop (Binop_array_access)) [trm_var_get x;trm_int i]) t1
      ) (Mlist.to_list tl) in
      let new_decl = trm_let_mut ~annot:t.annot (x, (get_inner_ptr_type tx)) (trm_uninitialized ?loc:init.loc ()) in
      trm_seq_nobrace_nomarks ([new_decl] @ array_set_list)
    | _ -> trm_fail init "detach_init_on: expected an array initialization"
    end
  | _ -> trm_fail t "detach_init_on: expected an array declaration"
