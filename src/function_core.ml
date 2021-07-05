open Ast
open Path

let bind_intro_aux (index : int) (fresh_name : var) (const : bool) (p_local : path) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     let lfront, lback = Tools.split_list_at index tl in
     let instr, lback = Tools.split_list_at 1 lback in
     let instr = List.hd instr in
     let trm_to_apply_changes, _ = Path.resolve_path p_local instr in
     let decl_to_insert = 
      if const then
        trm_let Var_immutable (fresh_name, typ_auto()) trm_to_apply_changes 
      else 
        trm_let Var_mutable (fresh_name, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_auto())) (trm_apps  (trm_prim (Prim_new (typ_auto()))) [trm_to_apply_changes])
      in  
     let decl_to_change = Generic_core.change_trm trm_to_apply_changes (trm_var fresh_name) instr in
     trm_seq ~annot:t.annot (lfront @ [decl_to_insert] @ [decl_to_change] @ lback)
  | _ -> fail t.loc "bind_intro_aux: expected the surrounding sequence"


let bind_intro (index : int) (fresh_name : var) (const : bool) (p_local : path) : Target.Transfo.local =
  Target.apply_on_path (bind_intro_aux index fresh_name const p_local)





let inline_delay_decl_aux (const : bool ) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let trm_to_change, lback = Tools.split_list_at 1 lback in
    let trm_to_change = List.hd trm_to_change in
    begin match trm_to_change.desc with 
    | Trm_let (_, (x, tx), _) ->
        let init_index = Tools.foldi (fun i acc t1 -> 
          match t1.desc with 
          | Trm_apps(_,[ls;_]) ->
            begin match ls.desc with 
            | Trm_var y when y = x -> Some i
            | _ -> acc
            end
          | _ -> acc
        ) None lback in
        let index1  = match init_index with 
        | Some index -> index
        | _ -> fail trm_to_change.loc "inline_delay_decl_aux: to assignment was found to the given variable"
          in
        let lfront1,lback1 = Tools.split_list_at index1 lback in
        let assgn_to_change,lback1  = Tools.split_list_at 1 lback1 in
        let assgn_to_change = List.hd assgn_to_change in
        begin match assgn_to_change.desc with 
        | Trm_apps(_, [_; rhs]) ->
          let vk = if const then Var_immutable else Var_mutable in
          let new_trm = trm_let vk (x, tx) (trm_apps (trm_prim (Prim_new tx)) [rhs]) in
          trm_seq ~annot:t.annot (lfront @ lfront1 @ [new_trm] @ lback1)
        | _ -> fail assgn_to_change.loc "inline_delay_decl_aux: something wen't wrong"
        end
    | _ -> fail t.loc "inline_delay_decl_aux: target_doesn't point to the right trm, expected a trm_let"
    end
  | _ -> fail t.loc "inline_delay_decl_aux: expected the surrounding sequence"


let inline_delay_decl (const : bool) (index : int) : Target.Transfo.local =
  Target.apply_on_path(inline_delay_decl_aux const index)


(* TODO: Remove goto trm for the last return instruction *)
let replace_return( exit_label : label) (r : var) (t : trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    match t.desc with 
    | Trm_abort ab ->
      begin match ab with 
      | Ret t1 ->
       begin match t1 with 
       | Some t2 ->
        begin match t2.typ with 
        | Some ty -> 
          begin match ty.typ_desc with 
          | Typ_unit -> trm_goto exit_label
          | _ -> trm_seq ~annot:(Some No_braces) [trm_set (trm_var r) t2; trm_goto exit_label]
          end
        | _ -> fail t.loc "replace_return: something went wrong"
        end
       | _ -> trm_labelled exit_label t
       end 
      | _ -> t
      end
    | _ -> trm_map (aux global_trm) t
  in aux t t

(* This function goes through every variable declaration and checks if this variable is already defined somewhere in the top level,
   if this is the case then this variable will be renamed inside the body of the function, after renaming 
   all its occurrences are changed *)
let change_variable_names (t : trm ) (surrounding_seq : trm) (rename : string -> string) : trm =
  match t. desc with 
  | Trm_seq tl ->
    List.fold_left (fun acc t1 ->
     match t1.desc with 
     | Trm_let (vk,(x, tx), init) -> 
       let find_prev_decl = Generic_core.toplevel_decl x surrounding_seq in
       begin match find_prev_decl with 
       | Some _ -> let acc = Generic_core.change_trm t1 (trm_let vk ((rename x), tx) init) acc in
          let acc = Generic_core.change_trm (trm_var x) (trm_var (rename x)) acc in acc
       | None -> acc 
       end
     | _ -> acc
    ) t tl

  | _ -> fail t.loc "change_variable_names: expected a function body declaration"


let inline_call_aux (index : int) (name : string) (label : string) (rename : string -> string) (top_ast : trm) (p_local : path ) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let trm_to_change, lback = Tools.split_list_at 1 lback in
    let trm_to_change = List.hd trm_to_change in
    let fun_call, _= Path.resolve_path p_local trm_to_change in
    let fun_call_name, fun_call_args = begin match fun_call.desc with 
                   | Trm_apps ({desc = Trm_var f; _}, args) -> f, args
                   | _ -> fail fun_call.loc "inline_call_aux: couldn't resolve the name of the function, target does not resolve to a function call"
                   end in
    let fun_decl = Generic_core.toplevel_decl fun_call_name top_ast in
    let fun_decl = begin match fun_decl with 
      | Some decl -> decl
      | None -> fail t.loc "inline_aux: no trm in top level gives the declaration with the given name"
      end in
    let fun_decl_type, fun_decl_args, fun_decl_body = begin match fun_decl.desc with
                   | Trm_let_fun (f, ty, args,body) when f = fun_call_name  -> ty, args, body
                   | _ -> fail fun_decl.loc "inline_call_aux: failed to find the top level declaration of the function"
                   end in
   let fun_decl_arg_vars = List.map trm_var (fst (List.split fun_decl_args)) in
   (* Since there is a chance that there can be argument which have the same name both on the function call and function definition,
      a replacing of the current args with the function call args with an underscore prefix is needed *)
   let fresh_args = List.map Generic_core.fresh_args fun_call_args in
   
   let fun_decl_body = List.fold_left2 (fun acc x y -> Generic_core.change_trm x y acc) fun_decl_body fun_decl_arg_vars fresh_args in
   
   let fun_decl_body = List.fold_left2 (fun acc x y -> Generic_core.change_trm x y acc) fun_decl_body fresh_args fun_call_args in
   
   let inlined_body = begin match fun_decl_type.typ_desc with 
                        | Typ_unit -> trm_seq ~annot:(Some No_braces) [
                            trm_labelled label (replace_return "__exit_body" name (change_variable_names fun_decl_body t rename ));
                            trm_labelled "__exit_body" (trm_var "")]
                        | _ -> trm_seq ~annot:(Some No_braces) [
                            trm_let Var_mutable (name, fun_decl_type) (trm_prim (Prim_new fun_decl_type));
                            trm_labelled label (replace_return "__exit_body" name (change_variable_names fun_decl_body t rename ));
                            trm_labelled "__exit_body" (trm_var "")]
                      end
                    in
      trm_seq ~annot:t.annot (lfront @ [inlined_body] @ lback)
          
  | _ -> fail t.loc "inline_call_aux: expected the surrounding sequence"


let inline_call (index: int) (name : string) (label : string) (rename : string -> string) (top_ast : trm) (p_local : path) : Target.Transfo.local = 
  Target.apply_on_path (inline_call_aux index name label rename top_ast p_local)