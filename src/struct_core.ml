open Ast


(* [set_explicit_aux field_list t]: This is an auxiliary function for set_explicit 
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
  
(* [set_explicit field_list t p] *)
let set_explicit (field_list : var list) : Target.Transfo.local =
  Target.apply_on_path(set_explicit_aux field_list)


(* [set_implicit t]: This is an auxiliary function for set_implicit
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

(* [set_implicit t p] *)
let set_implicit : Target.Transfo.local =
  Target.apply_on_path(set_implicit_aux)


(* [reorder_aux field_list: This function is an auxiliary function for reorder
    params:
      field_list: a list of fields given on a specific order
      subt: an ast subterm
    return: 
      the updated ast
 *)
let reorder_aux (struct_fields: var list) (move_where : string) (around : string) (t: trm) : trm =
  match t.desc with 
  | Trm_typedef td ->
   begin match td.typdef_body with 
   | Typdef_prod fs ->
    let field_list = 
    if move_where = "move_after" then
      Tools.move_fields_after around struct_fields fs
    else
      Tools.move_fields_before around struct_fields fs
    in
   trm_typedef {td with typdef_body = Typdef_prod fields_list}
  | _ -> fail t.loc "reorder_aux: expected a typdef_prod"
  end
  | _ -> fail t.loc "reorder_aux: expected a typedef definiton"

(* [reorder struct_fields move_where around t p] *)
let reorder (struct_fields : var list) (move_where : string) (around : string): Target.Transfo.local = 
  Target.apply_on_path(reorder_aux struct_fields move_where around)



(* Get the index for a given field of struct inside its list of fields *)
let get_pos (x : typvar) (t : trm) : int =
  begin match t.desc with
    | Trm_typedef {typdef_body = Typdef_prod fs; _} ->
        let rec find x lst =
        match lst with
        | [] -> raise (Failure "Not Found")
        | (hd, _) :: tl -> if hd = x then 0 else 1 + find x tl
        in
        find x fs
    | _ -> fail t.loc "get_pos_and_element: expected a struct type"
    end


let inline_record_access_core (var : string) (field : string) (struct_decl_trm : trm) (list_of_trms : trm list) (t : trm) : trm =
  
    (* search for the declaration of the variable *)
  let rec aux (global_trm : trm ) (t : trm) : trm =
      begin match t.desc with
      | Trm_apps (f,[base]) ->
        begin match f.desc with
        | Trm_val (Val_prim (Prim_unop (Unop_struct_access y)))
          | Trm_val (Val_prim (Prim_unop (Unop_struct_get y))) when y = field ->
          begin match base.desc with
          | Trm_var v when v = var ->
            let index = get_pos field struct_decl_trm in
            List.nth (List.rev list_of_trms) index
          | _ -> trm_map (aux global_trm) t
          end
        | _ -> trm_map (aux global_trm) t
        end
      | _ -> trm_map (aux global_trm) t
      end
    in aux t t