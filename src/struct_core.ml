open Ast



(* [set_explicit_aux field_list t]: This is an auxiliary function for set_explicit 
    params: 
      field_list: A string list, each string represents one 
      t: an ast subterm
    return: 
      the updated ast
 *)
let set_explicit_aux (t: trm) : trm =
  let typid_to_typedef_map = Clang_to_ast.(!ctx_typedef) in
  
  match t.desc with 
  | Trm_apps(_, [lt;rt]) ->
    let tid_r = Generic_core.get_typid rt  in 
    let tid_l = Generic_core.get_typid lt  in
    let tid = match tid_r, tid_l with 
    | -1, _ -> tid_l
    | _, -1 -> tid_r
    | _, _ -> if tid_r = tid_l then tid_r else fail t.loc "set_explicit_aux: different types in an assignment"
    in
    let struct_def = Typ_map.find tid typid_to_typedef_map in
    let field_list = Generic_core.get_field_list struct_def in
    begin match rt.desc with
    (* Get the type of the variables *)
     
    (* If the right hand side is a get *)
    | Trm_apps(f1, [rbase]) ->
      begin match lt.desc with
      | Trm_apps (f2, [lbase]) ->
        let exp_assgn = List.map (fun (sf, ty) ->
        let new_f = trm_unop (Unop_struct_access sf) in
         trm_set (trm_apps ~annot:(Some Access) ~typ:(Some ty) new_f [trm_apps ~annot:(Some Mutable_var_get) f2 [lbase]]) (trm_apps ~annot:(Some Access) ~typ:(Some ty) new_f [trm_apps ~annot:(Some Mutable_var_get) f1 [rbase]])
        ) field_list in
       trm_seq ~annot: (Some No_braces) exp_assgn

      | _ -> let exp_assgn = List.map(fun (sf, ty) ->
        let new_f = trm_unop (Unop_struct_access sf) in
        trm_set (trm_apps ~annot:(Some Mutable_var_get) ~typ:(Some ty) new_f [lt]) (trm_apps ~annot: (Some Access) ~typ:(Some ty) f1 [trm_apps ~annot:(Some Mutable_var_get) new_f [rbase]])
        ) field_list in 
        
        trm_seq ~annot:(Some No_braces) exp_assgn
      end
    (* If the right hand side is a struct initialization *)
    | Trm_struct st ->
      begin match lt.desc with 
      | Trm_apps (f2, lbase) ->
        let exp_assgn = List.mapi(fun i (sf, ty) ->
        let new_f = trm_unop (Unop_struct_access sf) in
        trm_set (trm_apps ~annot:(Some Access) ~typ:(Some ty) f2 [trm_apps ~annot:(Some Mutable_var_get) new_f lbase]) (List.nth st i)
        ) field_list in
        trm_seq ~annot: (Some No_braces) exp_assgn
      | Trm_var v ->
        let exp_assgn = List.mapi(fun i (sf, ty) ->
        let new_f = trm_unop (Unop_struct_access sf) in
        trm_set (trm_apps ~typ:(Some ty) new_f [trm_var v]) (List.nth st i)
        ) field_list in
        trm_seq ~annot: (Some No_braces) exp_assgn
      | _ -> fail t.loc "set_explicit_aux: left term was not matched"
      end
    | _ -> let exp_assgn = List.map (fun (sf, ty) ->
            let new_f = trm_unop (Unop_struct_access sf) in
              trm_set (trm_apps ~annot:(Some Access) ~typ:(Some ty) new_f [lt]) (trm_apps ~annot:(Some Access) ~typ:(Some ty) new_f [rt])
              ) field_list in
            trm_seq ~annot: (Some No_braces) exp_assgn
    end
  | Trm_let (vk, (x, tx), init) ->
    let inner_type = get_inner_ptr_type tx in
    let tyid = begin match inner_type.typ_desc with
    | Typ_constr (_, tid, _) -> tid
    | _-> fail t.loc "set_explicit_aux: expected a variable of construct type" 
    end in
    let struct_def = Typ_map.find tyid typid_to_typedef_map in
    let field_list = Generic_core.get_field_list struct_def in
    
    begin match init.desc with 
    | Trm_apps(_ , [base]) ->
      begin match base.desc with  
      | Trm_struct st ->
         let exp_assgn = List.mapi(fun i (sf, ty) ->
         let new_f = trm_unop (Unop_struct_access sf) in
          trm_set  (trm_apps ~typ:(Some ty) new_f  [trm_var x]) (List.nth st i)
        ) field_list in
         
          let var_decl = trm_let vk (x, tx) (trm_prim  (Prim_new (get_inner_ptr_type tx))) in
          trm_seq ~annot:(Some No_braces) ([var_decl] @ exp_assgn)
              
      | _ -> fail t.loc "set_explicit_aux:"
      end
    | _ -> fail t.loc "set_explicit_aux: spliting is no allowing for const variables"
    end
  | _ -> 
    
    fail t.loc "set_explicit_aux: this expression is not supported"
  
(* [set_explicit field_list t p] *)
let set_explicit : Target.Transfo.local =
  Target.apply_on_path(set_explicit_aux)


(* [set_implicit t]: This is an auxiliary function for set_implicit
    pararms:
      subt: an ast subterm
    return:
      the updated as
 *)
let set_implicit_aux (t: trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let first_instruction = List.hd tl in
    begin match first_instruction.desc with 
    | Trm_apps(f,[lhs;rhs]) ->
          begin match f.desc with 
          | Trm_val ( Val_prim ( Prim_binop Binop_set) ) -> 
            let lt = begin match lhs.desc with 
            | Trm_apps(f', [lt]) ->  
              begin match f'.desc with 
              | Trm_val (Val_prim (Prim_unop (Unop_struct_access _))) 
              | Trm_val (Val_prim (Prim_unop (Unop_struct_get _)))-> lt
              | _ -> fail f'.loc "set_implicit_aux: expected a struct access on the left hand side of the assignment"
              end
            | _ -> fail lhs.loc "set_implicit_aux: expected a struct access"
            end
            in 
            let rt = begin match rhs.desc with 
            | Trm_apps(f', [rt])  -> 
              begin match f'.desc with 
              | Trm_val ( Val_prim ( Prim_unop Unop_get ) ) ->
                begin match rt.desc with 
                | Trm_apps(f'',[rt]) ->
                  begin match f''.desc with 
                  | Trm_val (Val_prim (Prim_unop (Unop_struct_access _))) 
                      | Trm_val (Val_prim (Prim_unop (Unop_struct_get _)))-> rt
                      | _ -> fail f'.loc "set_implicit_aux: expected a struct acces on the right hand side of the assignment"
                  end
                | _ -> fail f'.loc "set_implicit_aux: expected a trm_apps" 
                end
                      
              | Trm_val (Val_prim (Prim_unop (Unop_struct_access _))) 
              | Trm_val (Val_prim (Prim_unop (Unop_struct_get _)))-> rt
              | _ -> fail f'.loc "set_implicit_aux: expected a struct acces on the right hand side of the assignment"
              end
            | _ -> fail rhs.loc "set_implicit_aux: expected a struct access"
            end
            in
            trm_set lt rt;
          | _ -> fail f.loc "set_explicit_aux: expected an assignment instruction" 
          end
      | _ -> fail t.loc "set_implicit_aux: expected a sequence with all explicit assignments"
        
    end
  | _ -> fail t.loc "set_implicit_aux: sequence which contains the set instructions was not matched"

(* [set_implicit t p] *)
let set_implicit : Target.Transfo.local =
  Target.apply_on_path(set_implicit_aux) 

let get_field_index (field : field) (fields : (var * typ) list) : int =
  let rec aux field fields c = match fields with 
    | [] -> failwith "get_field_index: empty list"
    | (f, _) :: tl -> 
      if (f = field) then c else aux field tl (c+1)
    in
  aux field fields 0


let inline_struct_access  (x : typvar) (t : trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, [base]) ->
      begin match f.desc with
      | Trm_val (Val_prim (Prim_unop (Unop_struct_access y)))
        | Trm_val (Val_prim (Prim_unop (Unop_struct_get y))) ->
          (* Removed this if else condition just for debugging purposes *)
          (* if false then fail t.loc ("Accessing field " ^ x ^ " is impossible, this field has been deleted during inlining")
          else  *)
          begin match base.desc with
          | Trm_apps (f',base') ->
            begin match f'.desc with

            | Trm_val(Val_prim (Prim_binop Binop_array_access))
              | Trm_val(Val_prim (Prim_binop Binop_array_get)) ->
                (* THen base caontains another base and also the index  *)
                let base2 = List.nth base' 0 in
                let index = List.nth base' 1 in
                begin match base2.desc with
                | Trm_apps(f'',base3) ->
                  begin match f''.desc with
                  | Trm_val (Val_prim (Prim_unop Unop_struct_access z))
                    | Trm_val (Val_prim (Prim_unop (Unop_struct_get z ))) when z = x ->
                    let new_var = z ^ "_" ^ y in
                    let new_f = {f' with desc = Trm_val(Val_prim (Prim_unop (Unop_struct_access new_var)))} in
                    trm_apps ~annot:t.annot  f' [trm_apps new_f base3;index]
                  | _ -> trm_map (aux global_trm) t
                  end
                | _ -> fail t.loc "inline_struct_access: expected a trm_apps"
                end

            | Trm_val (Val_prim (Prim_unop (Unop_struct_access z)))
              | Trm_val (Val_prim (Prim_unop (Unop_struct_get z))) when z = x ->
                let new_var = z ^"_"^ y in
                let new_f = {f' with desc = Trm_val(Val_prim (Prim_unop (Unop_struct_access new_var)))}
              in
              trm_apps ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement
                     ~add:t.add ~typ:t.typ new_f base'

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

let inline_struct_initialization (struct_name : string) (field_list : field list) (field_index : int) (t : trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    match t.desc with 
    | Trm_struct term_list ->
      begin match t.typ with 
      | Some { typ_desc = Typ_constr (y, _, _); _} when y = struct_name ->
        let trm_to_change = List.nth term_list field_index in
        begin match trm_to_change.desc with 
        | Trm_struct term_list_to_inline -> trm_struct (Tools.insert_sublist_in_list term_list_to_inline field_index term_list)
        | Trm_apps(_, [base]) ->
          begin match base.desc with 
          | Trm_var p ->
            let trm_list_to_inline = List.map(fun x ->
              trm_apps ~annot: (Some Access) (trm_unop (Unop_get))[
                trm_apps (trm_unop (Unop_struct_access x)) [
                  trm_var p
                ]
              ]
            ) field_list
            in
            trm_struct (Tools.insert_sublist_in_list trm_list_to_inline field_index term_list)
          | _ -> fail base.loc "inline_struct_initialization: expected a heap allocated variable"
          end 
        | _ -> trm_map (aux global_trm) t
        end
      | _ -> trm_map (aux global_trm) t
      end
    | _ -> trm_map (aux global_trm) t
  in
  aux t t

let inline_aux (field_to_inline : field) (index : int) (t : trm ) =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let td, lback = Tools.split_list_at 1 lback in
    let td = List.hd td in
    let node_context = td.ctx in
    let typid_to_typedef_map = begin match node_context with 
    | Some c ->  c.ctx_typedef
    | None -> fail t.loc "inline_aux: empty node context" 
    end
      in
    begin match td.desc with
    | Trm_typedef td ->
      begin match td.typdef_body with 
      | Typdef_prod (t_names, field_list) ->
       let field_index = get_field_index field_to_inline (List.rev field_list) in
       let lfront1, lback1 = Tools.split_list_at field_index (List.rev field_list) in
       let field_to_inline1, lback1 = if List.length lback1 = 1 then (lback1, []) else
        Tools.split_list_at 1 lback1 in
       let _ ,field_type = List.hd field_to_inline1 in
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
       let lback = List.map (inline_struct_access field_to_inline) lback in
       let lback = List.map (inline_struct_initialization td.typdef_tconstr (List.rev (fst (List.split (Generic_core.get_field_list struct_def)))) field_index) lback in
       trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)       
      | _ -> fail t.loc "inline_aux: expected a struct "
      end
    | _ -> fail t.loc "inline_aux: expected a trm_typedef"
    end
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"

let inline (field_to_inline : field) (index : int) : Target.Transfo.local =
  Target.apply_on_path (inline_aux field_to_inline index)


(* Auxiliary functions for reorder transformation *)

let get_pair x xs = List.fold_left(fun acc (y,ty) -> if y = x then (y,ty) :: acc else acc) [] xs

let get_pairs ys xs = List.fold_left(fun acc y -> (get_pair y xs) :: acc) [] ys

let remove_pair x xs = List.filter (fun (y,_) -> y <> x) xs

let remove_pairs (ys : var list) (xs : (var * typ) list) = List.fold_left (fun acc y -> remove_pair y acc) xs ys


let move_fields_after (x : var) (local_l : var list) (l : (var * typ) list) : (var * typ ) list=
  let fins = List.flatten (get_pairs local_l l )in
  let l = remove_pairs local_l l in 
  let rec aux = function
    | [] -> failwith "move_fields_after: ecmpty list" (* raise an error x not part of the list *)
    | (hd, ty) :: tl ->
      if hd = x
        then fins @ [hd, ty] @ tl (* local_l @ hd :: acc @ tl *)
        else aux tl
      in
    aux l

let move_fields_before (x : var) (local_l : var list) (l : (var * typ) list) : (var * typ) list =
  let fins = List.flatten (get_pairs local_l l) in
  let l = remove_pairs local_l l in
  let rec aux = function
    | [] -> failwith "move_fields_after: ecmpty list" (* raise an error x not part of the list *)
    | (hd, ty) :: tl ->
      if hd = x
        then [hd, ty] @ fins @ tl (* local_l @ hd :: acc @ tl *)
        else aux tl
      in
    aux l


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
   | Typdef_prod (tn, fs) ->
    let field_list = 
    if move_where = "move_after" then
      move_fields_after around struct_fields fs
    else
      move_fields_before around struct_fields fs
    in
   trm_typedef {td with typdef_body = Typdef_prod (tn, field_list)}
  | _ -> fail t.loc "reorder_aux: expected a typdef_prod"
  end
  | _ -> fail t.loc "reorder_aux: expected a typedef definiton"

(* [reorder struct_fields move_where around t p] *)
let reorder (struct_fields : var list) (move_where : string) (around : string): Target.Transfo.local = 
  Target.apply_on_path(reorder_aux struct_fields move_where around)



(* Get the index for a given field of struct inside its list of fields *)
let get_pos (x : typvar) (t : trm) : int =
  begin match t.desc with
    | Trm_typedef {typdef_body = Typdef_prod (_, fs); _} ->
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