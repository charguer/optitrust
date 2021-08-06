open Ast
open Path


let bind_intro_aux (index : int) (fresh_name : var) (const : bool) (p_local : path) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     let lfront, instr, lback = Internal.get_trm_and_its_relatives index tl in
     let trm_to_apply_changes, _ = Path.resolve_path p_local instr in
     let function_type = match trm_to_apply_changes.typ with 
     | Some typ -> typ
     (* Maybe it should fail here!! *)
     | None -> typ_auto() in
     let decl_to_insert = 
      if const then
        trm_let Var_immutable (fresh_name, function_type) trm_to_apply_changes 
      else 
        trm_let Var_mutable (fresh_name, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (function_type)) (trm_apps  (trm_prim (Prim_new (function_type))) [trm_to_apply_changes])
      in  
     let decl_to_change = Internal.change_trm trm_to_apply_changes (trm_var fresh_name) instr in
     trm_seq ~annot:t.annot (lfront @ [decl_to_insert] @ [decl_to_change] @ lback)
  | _ -> fail t.loc "bind_intro_aux: expected the surrounding sequence"


let bind_intro (index : int) (fresh_name : var) (const : bool) (p_local : path) : Target.Transfo.local =
  Target.apply_on_path (bind_intro_aux index fresh_name const p_local)


let nb_gotos = ref 0

let replace_return (exit_label : label) (r : var) (t : trm) : trm =
  let rec aux (is_terminal : bool) (t : trm) : trm =
    match t.desc with 
    | Trm_abort ab ->
      begin match ab with 
      | Ret t1 -> 
        begin match t1 with 
        | Some t2 ->
          let t1' = (aux false t2) in
          let t_assign = trm_set (trm_var r) t1' in
          if is_terminal 
            then t_assign
            else 
              begin 
              nb_gotos := !nb_gotos + 1;
              trm_seq [t_assign; trm_goto exit_label] 
              end
        | _ -> 
            let () = nb_gotos := !nb_gotos + 1 in
            trm_goto exit_label
        end
      | _ -> 
          nb_gotos := !nb_gotos + 1; 
          trm_goto exit_label
      end
    | _-> trm_map_with_terminal is_terminal aux t 
  in aux true t



(* This function goes through every variable declaration and checks if this variable is already defined somewhere in the top level,
   if this is the case then this variable will be renamed inside the body of the function, after renaming 
   all its occurrences are changed *)
let change_variable_names (t : trm ) (surrounding_seq : trm) (rename : string -> string) : trm =
  match t. desc with 
  | Trm_seq tl ->
    List.fold_left (fun acc t1 ->
     match t1.desc with 
     | Trm_let (vk,(x, tx), init) -> 
       let find_prev_decl = Internal.toplevel_decl x surrounding_seq in
       begin match find_prev_decl with 
       | Some _ -> let acc = Internal.change_trm t1 (trm_let vk ((rename x), tx) init) acc in
          let acc = Internal.change_trm (trm_var x) (trm_var (rename x)) acc in acc
       | None -> acc 
       end
     | _ -> acc
    ) t tl

  | _ -> fail t.loc "change_variable_names: expected a function body declaration"


let inline_call_aux (index : int) (label : string) (top_ast : trm) (p_local : path ) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, trm_to_change, lback = Internal.get_trm_and_its_relatives index tl in
    let fun_call, _= Path.resolve_path p_local trm_to_change in
    let fun_call_name, fun_call_args = begin match fun_call.desc with 
                   | Trm_apps ({desc = Trm_var f; _}, args) -> f, args
                   | _ -> fail fun_call.loc "inline_call_aux: couldn't resolve the name of the function, target does not resolve to a function call"
                   end in
    let fun_decl = Internal.toplevel_decl fun_call_name top_ast in
    let fun_decl = begin match fun_decl with 
      | Some decl -> decl
      | None -> fail t.loc "inline_aux: no trm in top level gives the declaration with the given name"
      end in
    let fun_decl_type, fun_decl_args, fun_decl_body = begin match fun_decl.desc with
                   | Trm_let_fun (f, ty, args,body) when f = fun_call_name  -> ty, args, body
                   | _ -> fail fun_decl.loc "inline_call_aux: failed to find the top level declaration of the function"
                   end in
   let fun_decl_arg_vars = List.map trm_var (fst (List.split fun_decl_args)) in
   (* Since there is a chance that there can be arguments which have the same name both on the function call and function definition,
      a replacing of the current args with the function call args with an underscore prefix is needed *)
   let fresh_args = List.map Internal.fresh_args fun_call_args in
   
   let fun_decl_body = List.fold_left2 (fun acc x y -> Internal.change_trm x y acc) fun_decl_body fun_decl_arg_vars fresh_args in
   let fun_decl_body = List.fold_left2 (fun acc x y -> Internal.change_trm x y acc) fun_decl_body fresh_args fun_call_args in
   
   let name = begin match trm_to_change.desc with 
              | Trm_let (_, (x, _), _) -> x
              | _ -> ""
              end in
   nb_gotos := 0;
   let labelled_body = begin match name with 
                       | "" -> 
                          trm_labelled label fun_decl_body 
                       | _ -> trm_labelled label (replace_return "__exit_body" name fun_decl_body)   
                       end in
   let exit_label = begin match !nb_gotos with
                    | 0  -> trm_lit (Lit_unit)
                    | _ -> trm_labelled "__exit_body" (trm_lit (Lit_unit)) 
                    end in
   let inlined_body = 
    begin match fun_decl_type.typ_desc with 
    | Typ_unit -> [labelled_body;exit_label]
    | _ ->  [trm_let Var_mutable (name, fun_decl_type) (trm_prim (Prim_new fun_decl_type));
              labelled_body;exit_label] 
    end in
       trm_seq ~annot:t.annot (lfront @ inlined_body @ lback)
  | _ -> fail t.loc "inline_call_aux: expected the surrounding sequence"


let inline_call (index: int) (label : string) (top_ast : trm) (p_local : path) : Target.Transfo.local = 
  Target.apply_on_path (inline_call_aux index label top_ast p_local)
