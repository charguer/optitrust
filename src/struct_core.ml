open Ast
open Target


(* set_explicit_aux: This is an auxiliary function for set_explicit 
    params: 
      field_list: A string list, each string represents one 
      t: an ast subterm
    return: 
      the updated ast
 *)
let set_explicit_aux (field_list : var list) (t: trm) : trm =
  match t.desc with 
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
       trm_seq ~annot: t.annot exp_assgn
      (* If the variable at the left hand side is not heap allocated *)
      | Trm_var v ->
        let exp_assgn = List.map(fun sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
        trm_set (trm_apps new_f [trm_var v]) (trm_apps ~annot: (Some Access) f1 [trm_apps new_f rbase])
        ) field_list in
        trm_seq ~annot:t.annot exp_assgn
      | _ -> fail t.loc "set_explicit_aux: left term was not matched"
      end
    (* If the right hand side is a struct initialization *)
    | Trm_struct st ->
      begin match lt.desc with 
      | Trm_apps (f2, lbase) ->
        let exp_assgn = List.mapi(fun i sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
        trm_set (trm_apps ~annot:(Some Access) f2 [trm_apps new_f lbase]) (List.nth st i)
        ) field_list in
        trm_seq ~annot:t.annot exp_assgn
      | Trm_var v ->
        let exp_assgn = List.mapi(fun i sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
        trm_set (trm_apps new_f [trm_var v]) (List.nth st i)
        ) field_list in
        trm_seq ~annot:t.annot exp_assgn
      | _ -> fail t.loc "set_explicit_aux: left term was not matched"
      end
    (* Any othe expression *)
    | _ -> 
      let exp_assgn = List.map(fun sf ->
      let new_f = trm_unop (Unop_struct_get sf) in
      trm_set (trm_apps new_f [lt]) (trm_apps new_f [rt])
      ) field_list in
      trm_seq exp_assgn
    end
  | _ -> fail t.loc "set_explicit_aux: this expression is not supported"
  
(* TODO: Ask Arthur if the user should give the list of fields or we should try to find it automatically *)
(* set_explicit: Transoform a struct set instruction to multiple struct set field instructions   
    params:
      field_list:
        A list of struct field names, used for the explicit assignments
      path_to_set: 
        Path to the set instruction
      t: ast
    return:
      the updated ast
 *)
let set_explicit (field_list : var list) : Transfo.local =
  Target.apply_on_path(set_explicit_aux field_list)


(* set_implicit: This is an auxiliary function for set_implicit
    pararms:
      subt: an ast subterm
    return:
      the updated as
 *)
let set_implicit_aux (t: trm) : trm =
  match t.desc with 
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
          | _ -> fail t.loc "set_implicit_aux: expected a heap stack allocated variable access, other expressions for the moment are not supported"
        end
      in
      let r_var = begin match  rs.desc with 
      | Trm_apps(_,[base]) -> base
      | Trm_var x -> trm_var x
      | _ -> fail t.loc "set_implicit_aux: expected a heap stack allocated variable access, other expressions for the moment are not supported"
      end
      in
      trm_set l_var r_var
    | _ -> fail t.loc "set_implicit_aux: expected a set instruction"
    end
  | _ -> fail t.loc "set_implicit_aux: sequence which contains the set instructions was not matched"

(* set_implicit: Transoform a sequence of set instructions into a single set instruction   
    params:
      path_to_seq: 
        Path to the sequence containing the set instructions
      t: ast
    return:
      the updated ast
 *)
let set_implicit : Transfo.local =
  Target.apply_on_path(set_implicit_aux)


(* reorder_aux: This function is an auxiliary function for reorder
    params:
      field_list: a list of fields given on a specific order
      subt: an ast subterm
    return: 
      the updated ast
 *)
let reorder_aux (field_list : var list) (t: trm) : trm =
  match t.desc with 
      | Trm_typedef (Typedef_abbrev (x, dx)) ->

        let field_map =
          match dx.ty_desc with
            | Typ_struct(_,m,_) -> m
            |_ -> fail t.loc "reorder_aux: the type should be a typedef struct"
          in
        
        trm_typedef (Typedef_abbrev (x, typ_struct field_list field_map x))
      | _ -> fail t.loc "reorder_aux: expected a typedef definiton"


(* reorder: Reorder fields of a typedef struct
    params:
      field_list: a list of fields given on a specific order
      path_to_struct: path to the typdef struct 
      t: ast
    return:
      the updated ast
 *)
let reorder (field_list : var list) : Transfo.local = 
  Target.apply_on_path(reorder_aux field_list)


