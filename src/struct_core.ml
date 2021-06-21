open Ast



(* [set_explicit_aux field_list t]: This is an auxiliary function for set_explicit 
    params: 
      field_list: A string list, each string represents one 
      t: an ast subterm
    return: 
      the updated ast
 *)
let set_explicit_aux (t: trm) : trm =
  let node_context = t.ctx in
  let typid_to_typedef_map  = begin match node_context with 
  | Some c -> c.ctx_typedef
  | None -> fail t.loc "set_explicit_aux, empty node context"
  end
  in  
  match t.desc with 
  | Trm_apps(_, [lt;rt]) ->
    (* Ast_to_text.print_ast ~only_desc:true stdout rt; *)
    let tid = Generic_core.get_typid rt  in
    Tools.printf "Got typid %d\n" tid;
    let struct_def = Typ_map.find tid typid_to_typedef_map in
    let field_list = List.rev (Generic_core.get_field_list struct_def) in
    begin match rt.desc with
    (* Get the type of the variables *)
     
    (* If the right hand side is a get *)
    | Trm_apps(f1, [rbase]) ->
      begin match lt.desc with
      (* If the variable and the left hand side is heap allocated*)
      | Trm_apps (f2, [lbase]) ->
        let exp_assgn = List.map (fun sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
         trm_set (trm_apps ~annot:(Some Access) new_f [trm_apps f2 [lbase]]) (trm_apps ~annot:(Some Access) new_f [trm_apps f1 [rbase]])
        ) field_list in
       trm_seq ~annot: t.annot exp_assgn
      (* If the variable at the left hand side is not heap allocated *)
      | Trm_var v ->
        let exp_assgn = List.map(fun sf ->
        let new_f = trm_unop (Unop_struct_get sf) in
        trm_set (trm_apps new_f [trm_var v]) (trm_apps ~annot: (Some Access) f1 [trm_apps new_f [rbase]])
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
    | _ -> fail t.loc "set_explicit_aux: other expressions are not supported" 
      (* let exp_assgn = List.map(fun sf ->
      let new_f = trm_unop (Unop_struct_get sf) in
      trm_set (trm_apps new_f [lt]) (trm_apps new_f [rt])
      ) field_list in
      trm_seq exp_assgn *)
    end
  | _ -> fail t.loc "set_explicit_aux: this expression is not supported"
  
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