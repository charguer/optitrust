open Ast
open Path
open Ast_to_c
open Transformations
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


(*  t[i][k]  t:x   then swap dimentions for t

  transformation to swap the two first dimensions of an array
  name is used to name function copies
  assumption: x is a type variable that represents a multidimensional array type
  with >= 2 dimensions
  all variables of type x will be swapped
  assumption: x is not used in fun declarations
*)

(*
  swap the dimensions in the declaration of x
  swap all the accesses to arrays of type x
 *)
let rec swap_accesses (clog : out_channel) (x : typvar) (t : trm) : trm =
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
                    | Some {ty_desc = Typ_var x'; _} when x' = x ->
                       (* x might also be the type of arrays in indices… *)
                       let swapped_index = swap_accesses clog x index in
                       let swapped_index' = swap_accesses clog x index' in
                       trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr
                         ~typ:t.typ f
                         [
                           trm_apps ~annot:base.annot ~loc:base.loc
                             ~is_instr:base.is_instr ~typ:base.typ f'
                             [
                               base';
                               swapped_index
                             ];
                           swapped_index'
                         ]
                    (*
                      otherwise we recursively call swap_accesses after removing
                      one dimension
                     *)
                    | _ ->
                       let swapped_l = List.map (swap_accesses clog x) tl in
                       trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr
                         ~typ:t.typ f swapped_l
                    end
                 | _ ->
                    fail f'.loc ("swap_coordinates: array accesses should " ^
                                   "have 2 arguments");
                 end
              (*
                again, if we do not find two successive accesses, we
                recursively call swap_accesses
               *)
              | _ ->
                 let swapped_l = List.map (swap_accesses clog x) tl in
                 trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr f
                   ~typ:t.typ swapped_l
              end
           (* again, … *)
           | _ ->
              let swapped_l = List.map (swap_accesses clog x) tl in
              trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr f
                ~typ:t.typ swapped_l
           end
        | _ -> fail f.loc ("swap_coordinates: array accesses should have 2 " ^
                             "arguments");
        end
     (*
         for most other terms we only recursively call swap_accesses
         note: arrays of type x might appear in f now
      *)
     | _ ->
        let swapped_f = swap_accesses clog x f in
        let swapped_l = List.map (swap_accesses clog x) tl in
        trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~typ:t.typ
          swapped_f swapped_l
     end
  (* declaration… *)
  | Trm_decl d ->
     begin match d with
     (* we look for the declaration of x *)
     | Def_typ (y, ty) when y = x ->
        let log : string =
          Printf.sprintf
           ("  - type\n%s\n" ^^
            "    is a multidimensional array type\n"
           )
           (typ_to_string ty)
        in
        write_log clog log;
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
        trm_decl ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr ~add:t.add
           (Def_typ (y, swap_type ty))
     (*
         all the interesting cases are covered now, we only have to do recursive
         calls
         var and fun decl first
      *)
     | _ -> trm_map (swap_accesses clog x) t
     end
  (*
     remaining cases: val, var, array, struct, if, seq, while, for, switch,
     abort, labelled
     inside values, array accesses may only happen in array sizes in types
     todo: currently ignored, is it reasonable to expect such things to happen?
   *)
  | _ -> trm_map (swap_accesses clog x) t

and swap_coord (clog : out_channel) (name : var -> var) (x : typvar) (t : trm) : trm =
  (*
    3 things to change in t:
    - dimensions in the declaration of x
    - add a copy of each function taking an argument of type x and replace the
    function calls with these copies
    - array accesses on variables of type x
      includes initialisation (through loop)
      --> no initialisation by list for multidimensional arrays
   *)
  let ilsm = functions_with_arg_type x t in
  (* first add copies of the functions *)
  let t = insert_fun_copies name ilsm x t in
  (* then replace function calls *)
  let t = replace_fun_names name ilsm x t in
  (* finally adapt the declaration and accesses *)
  swap_accesses clog x t


(*
  Array of Structures to Structure of Arrays:²²²
  if s is struct {t1 field1; …; tm fieldm} and x is s[n], transforms x into s'
  where s' is struct {t1 field1[n]; …; tm fieldm[n]}
  arguments:
    - x: type variable representing the array type
    - name: to name function copies
  assumptions:
    - x is not used in function definitions, but only in var declarations
 *)

let swap_accesses (clog : out_channel) (x : typvar) (t : trm) : trm =
  let rec aux (global_trm : trm) (t :trm) : trm =
    match t.desc with
    (* declarations *)
    | Trm_decl d ->
       begin match d with
       (* we have to change the declaration of x *)
       | Def_typ (y, ty) when y = x ->
        let log : string =
          Printf.sprintf
           ("  - type\n%s\n" ^^
            "    is an array type\n"
           )
           (typ_to_string ty)
        in
        write_log clog log;
          (*
            ty must be an array type over a struct type denoted by a type var
           *)
          begin match ty.ty_desc with
          | Typ_array ({ty_desc = Typ_var y; _}, s) ->
             begin match aliased_type y global_trm with
             | None ->
                fail t.loc "swap_accesses: cannot find underlying struct type"
             | Some ty' ->
                let log : string =
                  Printf.sprintf
                    ("  - type\n%s\n" ^^
                       "    is a struct type\n"
                    )
                    (typ_to_string ty')
                in
                write_log clog log;
                begin match ty'.ty_desc with
                | Typ_struct (l,m, n) ->
                   let m =
                     Field_map.map
                       (fun ty'' ->
                         typ_array ~ty_attributes:ty.ty_attributes ty'' s) m
                   in
                   trm_decl (Def_typ (x, typ_struct l m n))
                | _ ->
                   fail t.loc "swap_accesses: expected underlying struct type"
                end
             end
          | _ -> fail t.loc "swap_accesses: expected array type declaration"
          end
       (*
         other cases: type declarations (not x), fun declarations, var
         declarations (not of type x)
         arrays of type x are heap allocated
        *)
       | _ -> trm_map (aux global_trm) t
       end
    (* accesses: y[i].f becomes (y.f)[i] *)
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
                begin match base'.typ with
                | Some {ty_desc = Typ_var y; _} when y = x ->
                   (* x might appear both in index and in base' *)
                   let base' = aux global_trm base' in
                   let index = aux global_trm index in
                   (* keep outer annotations *)
                   trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr
                     ~add:t.add ~typ:t.typ f' [trm_apps f [base']; index]
                | Some {ty_desc = Typ_ptr {ty_desc = Typ_var y; _}; _}
                     when y = x ->
                   (* x might appear both in index and in base' *)
                   let base' = aux global_trm base' in
                   let index = aux global_trm index in
                   (* keep outer annotations *)
                   trm_apps ~annot:t.annot ~loc:t.loc ~is_instr:t.is_instr
                     ~add:t.add ~typ:t.typ f' [trm_apps f [base']; index]
                | _ -> trm_map (aux global_trm) t
                end
             | _ -> trm_map (aux global_trm) t
             end
          | _ -> trm_map (aux global_trm) t
          end
       | _ -> trm_map (aux global_trm) t
       end
    (* other cases: recursive call *)
    | _ -> trm_map (aux global_trm) t
  in
  aux t t

let aos_to_soa (clog : out_channel) (name : var -> var) (x : typvar)
  (t : trm) : trm =
  (*
    changes:
    - declaration of x
    - add a copy of each function taking an argument of type x and replace the
    function calls with these copies
    - accesses on variables of type x
   *)
  let ilsm = functions_with_arg_type x t in
  (* first add copies of the functions *)
  let t = insert_fun_copies name ilsm x t in
  (* then replace function calls *)
  let t = replace_fun_names name ilsm x t in
  (* finally adapt the declaration and accesses *)
  swap_accesses clog x t


let array_to_variables_aux (clog : out_channel) (new_vars : var list) (decl_trm : trm) (decl_index : int) (t  : trm) : trm =  
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
              | _ -> fail t.loc "array_to_variables_aux: expected a type variable"
              end 
            | _ -> fail t.loc "array_to_variables_aux: expected an array type"
            end 
          | _ -> fail t.loc "array_to_variables_aux: something went wrong"
          end 
        | _ -> fail t.loc "array_to_variables_aux: expected a variable declaration"
        end 
      | _ -> fail t.loc "array_to_variables_aux: expected a sequence which contain the variable declaration"
      end 
      in 
      (* let decl_index = get_index decl_trm tl in *)
      let new_trms = List.map(fun x -> 
        trm_seq ~annot:(Some Heap_allocated) [trm_decl (Def_var((x,typ_ptr (typ_var decl_type)),trm_prim (Prim_new (typ_var decl_type))))]) new_vars 
      in    
      trm_seq ~annot:t.annot (insert_sublist_in_list new_trms decl_index tl) 
    | _ -> fail t.loc "array_to_variables_aux: only declaration inside sequence are supported"

let inline_array_access (clog : out_channel) (array_var : var) (new_vars : var list) (t: trm) : trm = 
  let log : string = 
    let loc : string =
    match t.loc with 
    | None -> ""
    | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in Printf.sprintf
    (" - expression\n%s\n" ^^
    " %s is the full term\n"
    )
    (ast_to_string t) loc 
    in write_log clog log;
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
      
         
let array_to_variables (clog : out_channel) (dcl_path : path list) (new_vars : var list) (t : trm) : trm = 
  let p = List.flatten dcl_path in 
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_path p t in 
  Flags.verbose := b;
  let app_transfo (t :trm) (dl : expl_path) : trm = 
    match List.rev dl with 
    | Dir_nth n :: dl' -> 
      let (t',_) = resolve_explicit_path dl t in
      let dl = List.rev dl' in
      
      apply_local_transformation (array_to_variables_aux clog new_vars t' n) t dl 
    | _ -> fail t.loc "app_transfo: expected a dir_nth inside the sequence"
  in 
  (* Change all array accessess with the new variables before changing the declaration *)
  let declaration_trm = match epl with 
  | [dl] -> let (t_def,_) = resolve_explicit_path dl t in t_def 
  | _ -> fail t.loc "array_to_variables: expected only one declaration trm"
  in 
  let array_variable = match declaration_trm.desc with
  | Trm_seq [{desc=Trm_decl (Def_var ((x,_),_));_}] -> x 
  | _ -> fail t.loc "array_to_variables: expected a sequece which contains the declration"
  in 
  let t = inline_array_access clog array_variable new_vars t in 
  match epl with 


  | [] ->
    print_info t.loc "array_to_variables: no matching subterm";
    t
  | _ -> List.fold_left(fun t dl -> app_transfo t dl)
    t epl 