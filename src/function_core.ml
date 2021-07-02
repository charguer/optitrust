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
        (* TODO: Fix the issue with the star appearing when using auto with pointer *)
        trm_let Var_mutable (fresh_name, typ_auto()) (trm_apps  (trm_prim (Prim_new (typ_auto()))) [trm_to_apply_changes])
      in  
     let decl_to_change = Generic_core.change_trm trm_to_apply_changes (trm_var fresh_name) instr in
     trm_seq ~annot:t.annot (lfront @ [decl_to_insert] @ [decl_to_change] @ lback)
  | _ -> fail t.loc "bind_intro_aux: expected the surrounding sequence"


let bind_intro (index : int) (fresh_name : var) (const : bool) (p_local : path) : Target.Transfo.local =
  Target.apply_on_path (bind_intro_aux index fresh_name const p_local)


let inline_call_aux (index : int) (name : string) (label : string) (top_ast : trm) (p_local : path ) (t : trm) : trm =
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
  (* Replace function declaration args with function call args *)
   let fun_decl_body = List.fold_left2 (fun acc x y -> Generic_core.change_trm x y acc) fun_decl_body fun_decl_arg_vars fun_call_args in
   
   let inlined_body = begin match fun_decl_body.desc with 
                      | Trm_seq tl1 ->
                        
                        begin match List.rev tl1 with 
                         | [] -> fail fun_decl_body.loc "inline_call_aux: empty function body"
                         | hd :: tl2 -> 
                          
                          begin match fun_decl_type.typ_desc with 
                          | Typ_unit   ->
                              trm_seq ~annot:(Some No_braces) [
                              trm_labelled label (trm_seq (List.rev tl2 @ [trm_goto "__exit_body"]));
                              trm_labelled "__exit_body" (trm_var "")
                              ] 
                          | _ -> 
                            
                            let ret = begin match hd.desc with
                                      | Trm_abort (Ret ret) ->
                                        begin match ret with 
                                        | Some t -> t
                                        | None -> fail hd.loc "inline_call_aux: expected a non-void function"
                                        end
                                      | _ -> fail hd.loc "inline_call_aux: expcted a return instruction"
                                      end
                            in
                            trm_seq ~annot:(Some No_braces) [
                              trm_let Var_mutable (name, fun_decl_type) (trm_prim (Prim_new fun_decl_type));
                              trm_labelled label (trm_seq (List.rev tl2 @ [trm_set (trm_var name) ret]));
                            ]
                          end
                        end

                      | _ -> fail fun_decl_body.loc "inline_call_aux: body of the function declaration should be a sequence"
                      end in
      trm_seq ~annot:t.annot (lfront @ [inlined_body] @ lback)
          
  | _ -> fail t.loc "inline_call_aux: expected the surrounding sequence"


let inline_call (index: int) (name : string) (label : string) (top_ast : trm) (p_local : path) : Target.Transfo.local = 
  Target.apply_on_path (inline_call_aux index name label top_ast p_local)