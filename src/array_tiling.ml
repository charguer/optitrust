open Ast
open Transformations
open Translate_ast

(*
  array tiling: transforms an array t[n] into a matrix t[n/b][b] for a fixed
  block size b
  name the block's type as block_name
  arguments:
    - x: type variable representing the array type
      x may be an alias for ty[n] or for ty* where ty is the type of the array
      elements
    - b: block size
  assumptions:
    - if x is ty*, each array of type x is allocated through a custom function:
      x a = my_alloc(nb_elements, size_element)
    - x is not used in function definitions, but only in var declarations
    - for now: in any case, the number of elements is divisible by b
 *)

let rec tile_aux (base_type : typ) (block_name : typvar) (b : trm) (x : typvar)
  (t : trm) : trm =
  (*
    replace sizeof(base_type) with sizeof(block_name)
    if another term is used for size: use b * t_size
   *)
  let new_size (t_size : trm) : trm =
    if Translate_ast.ast_to_string t_size =
         "sizeof(" ^ Translate_ast.typ_to_string base_type ^ ")"
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
           | Undefined -> fail t.loc "tile_aux: array size must be provided"
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
        | _ -> fail t.loc "tile_aux: expected array or pointer type declaration"
        end
     (*
       other cases: type declarations (not x), fun declarations, var
       declarations (not of type x)
       arrays of type x are heap allocated
      *)
     | _ -> trm_map (tile_aux base_type block_name b x) t
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
     | _ -> trm_map (tile_aux base_type block_name b x) t
     end
  (* set with alloc *)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
              [lhs; rhs]) ->
     (* lhs should have type x *)
     begin match lhs.typ with
     | Some {ty_desc = Typ_var y; _} when y = x ->
        trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~add:t.add
          ~typ:t.typ (trm_binop Binop_set) [lhs; new_alloc rhs]
     | _ -> trm_map (tile_aux base_type block_name b x) t
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
              trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~add:t.add
                ~typ:t.typ f
                [
                  trm_apps ~annot:base.annot ~loc:base.loc ~is_instr:false
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
           | _ -> trm_map (tile_aux base_type block_name b x) t
           end
        | _ -> fail t.loc "tile_aux: array accesses must have two arguments"
        end
     | _ -> trm_map (tile_aux base_type block_name b x) t
     end
  | _ -> trm_map (tile_aux base_type block_name b x) t

let tile (clog : out_channel) (name : var -> var) (block_name : typvar)
  (b : trm) (x : typvar) (t : trm) : trm =
  (*
    changes:
      - replace the definition of x with:
        + (ty[b])* if x is ty*
        + ty[n/b][b] if x is ty[n]
        in both cases: define ty[b] as block_name
      - add a copy of each function taking an argument of type x and replace the
        function calls with these copies
      - replace array accesses a[i] for a of type x with a[i/b][i%b]
   *)
  let ilsm = functions_with_arg_type x t in
  (* first add copies of the functions *)
  let t = insert_fun_copies name ilsm x t in
  (* then replace function calls *)
  let t = replace_fun_names name ilsm x t in
  (* finally adapt the declaration and accesses *)
  let base_type =
    match aliased_type x t with
    | None -> fail t.loc "tile: unable to find array type"
    | Some ty ->
       let log : string =
         Printf.sprintf
           ("  - type\n%s\n" ^^
            "    represents a one-dimensional array type or a pointer type\n"
           )
           (typ_to_string ty)
       in
       write_log clog log;
       begin match ty.ty_desc with
       | Typ_ptr ty ->
          let log : string =
            Printf.sprintf
              ("  - each array of type %s is declared using the following " ^^
                 "pattern:\n" ^^
               "      %s array_name = my_alloc(nb_elements, size_element)\n" ^^
               "  where nb_elements is divisible by %s\n"
              )
              x x (ast_to_string b)
          in
          write_log clog log;
          ty
       | Typ_array (ty, s) ->
          let log : string =
            let n : string =
              match s with
              | Undefined ->  fail t.loc "tile: array size must be provided"
              | Const n -> string_of_int n
              | Trm t -> ast_to_string t
            in
            Printf.sprintf "  - size %s is divisible by %s\n" n
              (ast_to_string b)
          in
          write_log clog log;
          ty
       | _ -> fail t.loc "tile: expected array or pointer type"
       end
  in
  clean_up_no_brace_seq (tile_aux base_type block_name b x t)
