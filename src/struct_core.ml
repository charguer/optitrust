open Ast
open Target


(* struct_set_explicit_aux: This is an auxiliary function for struct_set_explicit 
    params: 
      field_list: A string list, each string represents one 
      subt: an ast subterm
    return: 
      the updated ast
 *)
let struct_set_explicit_aux (field_list : var list) (subt : trm) : trm =
  match subt.desc with 
  | Trm_apps(_, [lt;rt]) ->
    begin match rt.desc with
    (* If the right hand side is a get *)
    | Trm_apps(f1, rbase) ->
      begin match lt.desc with
      (* If the variable and the left hand side is heap allocated*)
      | Trm_apps (f2, lbase) ->
        let exp_assgn = List.map (fun sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
         trm_set (trm_apps ~annot:(Some Access) new_f [trm_apps f2 lbase]) (trm_apps ~annot:(Some Access) new_f [trm_apps f1 rbase])
        ) field_list in
       trm_seq ~annot: subt.annot exp_assgn
      (* If the variable at the left hand side is not heap allocated *)
      | Trm_var v ->
        let exp_assgn = List.map(fun sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
        trm_set (trm_apps new_f [trm_var v]) (trm_apps ~annot: (Some Access) f1 [trm_apps new_f rbase])
        ) field_list in
        trm_seq ~annot:subt.annot exp_assgn
      | _ -> fail subt.loc "struct_set_explicit_aux: left term was not matched"
      end
    (* If the right hand side is a struct initialization *)
    | Trm_struct st ->
      begin match lt.desc with 
      | Trm_apps (f2, lbase) ->
        let exp_assgn = List.mapi(fun i sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
        trm_set (trm_apps ~annot:(Some Access) f2 [trm_apps new_f lbase]) (List.nth st i)
        ) field_list in
        trm_seq ~annot:subt.annot exp_assgn
      | Trm_var v ->
        let exp_assgn = List.mapi(fun i sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
        trm_set (trm_apps new_f [trm_var v]) (List.nth st i)
        ) field_list in
        trm_seq ~annot:subt.annot exp_assgn
      | _ -> fail subt.loc "struct_set_explicit_aux: left term was not matched"
      end
    (* Any othe expression *)
    | _ -> 
      let exp_assgn = List.map(fun sf ->
      let new_f = trm_unop (Unop_struct_get sf) in
      trm_set (trm_apps new_f [lt]) (trm_apps new_f [rt])
      ) field_list in
      trm_seq exp_assgn
    end
  | _ -> fail subt.loc "make_explicit_record_assignment_aux: this expression is not supported"
  
(* TODO: Ask Arthur if the user should give the list of fields or we should try to find it automatically *)
(* struct_set_explicit: Transoform a struct set instruction to multiple struct set field instructions   
    params:
      field_list:
        A list of struct field names, used for the explicit assignments
      path_to_set: 
        Path to the set instruction
      t: ast
    return:
      the updated ast
 *)
let struct_set_explicit (field_list : var list) (path_to_set : path) (t : trm) : trm =
  apply_local_transformation(struct_set_explicit_aux field_list) t path_to_set


(* struct_set_implicit: This is an auxiliary function for struct_set_implicit
    pararms:
      subt: an ast subterm
    return:
      the updated as
 *)
let struct_set_implicit_aux (subt : trm) : trm =
  match subt.desc with 
  | Trm_seq tl ->
    (* To find out the variables of the structs whe just conside the first set instruction.
      Assumption: The sequence contains only the struct field set instructions.
     *)
    let first_instruction = List.hd tl in
    begin match first_instruction.desc with
    | Trm_apps(_,[ls;rs]) ->
      (* Find left variable, seach for expression of the form v1.x  *)
      let l_var = 
        begin match  ls.desc with 
          | Trm_apps(_,[base]) -> base
          | Trm_var x -> trm_var x
          | _ -> fail subt.loc "struct_set_implicit_aux: expected a heap stack allocated variable access, other expressions for the moment are not supported"
        end
      in
      let r_var = begin match  rs.desc with 
      | Trm_apps(_,[base]) -> base
      | Trm_var x -> trm_var x
      | _ -> fail subt.loc "struct_set_implicit_aux: expected a heap stack allocated variable access, other expressions for the moment are not supported"
      end
      in
      trm_set l_var r_var
    | _ -> fail subt.loc "struct_set_implicit_aux: expected a set instruction"
    end
  | _ -> fail subt.loc "struct_set_implicit_aux: sequence which contains the set instructions was not matched"

(* struct_set_implicit: Transoform a sequence of set instructions into a single set instruction   
    params:
      path_to_seq: 
        Path to the sequence containing the set instructions
      t: ast
    return:
      the updated ast
 *)
let struct_set_implicit (path_to_set : path) (t : trm) : trm =
  apply_local_transformation(struct_set_implicit_aux ) t path_to_set


(* struct_reorder_aux: This function is an auxiliary function for struct_reorder
    params:
      field_list: a list of fields given on a specific order
      subt: an ast subterm
    return: 
      the updated ast
 *)
let struct_reorder_aux (field_list : var list) (subt : trm) : trm =
  match subt.desc with 
      | Trm_typedef (Typedef_abbrev (x, dx)) ->

        let field_map =
          match dx.ty_desc with
            | Typ_struct(_,m,_) -> m
            |_ -> fail subt.loc "fields_reorder: the type should be a typedef struct"
          in
        
        trm_typedef (Typedef_abbrev (x, typ_struct field_list field_map x))
      | _ -> fail subt.loc "fields_reorder: expected a typedef definiton"


(* struct_reorder: Reorder fields of a typedef struct
    params:
      field_list: a list of fields given on a specific order
      path_to_struct: path to the typdef struct 
      t: ast
    return:
      the updated ast
 *)
let struct_reorder (field_list : var list) (path_to_struct : path) (t : trm): trm = 
  apply_local_transformation(struct_reorder_aux field_list) t path_to_struct


