open Ast
open Path

(* [bind_intro_aux my_mark index fresh_name vonst p_local t]: bind the variable [fresh_name] to the targeted function call, 
      [my_mark] - put a mark on the targeted function call, 
      [index] - index of the instruction that contains the targeted function call on its surrouding sequence,
      [const] - flag for the mutability of the binded variable,
      [p_local] - path from the instruction containing the function call to the function call itself,
      [t] - ast of the sequence that contains the targeted function call. *)
let bind_intro_aux (my_mark : string) (index : int) (fresh_name : var) (const : bool) (p_local : path) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     let lfront, instr, lback = Internal.get_trm_and_its_relatives index tl in
     let function_call = Path.resolve_path p_local instr in
     let has_reference_type = if (Str.string_before fresh_name 1) = "&" then true else false in
     let fresh_name = if has_reference_type then (Str.string_after fresh_name 1) else fresh_name in

     let function_type = match function_call.typ with
     | Some typ -> typ
     | None -> typ_auto() in
     let change_with = (trm_var_possibly_mut ~const ~typ:(Some function_type) fresh_name) in
     let decl_to_change = Internal.change_trm function_call change_with instr in

     let function_call = trm_add_mark my_mark function_call in
     let decl_to_insert =
      if const
        then trm_let_immut (fresh_name, function_type) function_call
        else trm_let_mut (fresh_name, function_type) function_call
      in
     let new_tl = Mlist.merge lfront (Mlist.of_list ([decl_to_insert] @ [decl_to_change])) in
     let new_tl = Mlist.merge new_tl lback in
     let res = trm_seq ~annot:t.annot new_tl in
     res
  | _ -> fail t.loc "Function_core.bind_intro_aux: expected the surrounding sequence"

(* [bind_intro ~my_mark index fresh_name const p_local]: applies [bind_intro_aux] at the trm with path [p]. *)
let bind_intro ?(my_mark : string =  "") (index : int) (fresh_name : var) (const : bool) (p_local : path) : Target.Transfo.local =
  Target.apply_on_path (bind_intro_aux my_mark index fresh_name const p_local)


(* [replace_return exit_label r t]: removes all the return statements from the body of a function declaration,
      [exit_label] - generated only if [t] is there is a sequence that contains not terminal instructions,
      [r] - the name of the variable replacing the return statement,
      [t] - ast of the body of the function. *)
let process_return_in_inlining (exit_label : label) (r : var) (t : trm) : (trm * int) =
  let nb_gotos = ref 0 in
  let rec aux (is_terminal : bool) (t : trm) : trm =
    match t.desc with
    | Trm_abort ab ->
      begin match ab with
      | Ret t1 ->
        begin match t1 with
        | Some t2 ->
          let t1' = (aux false t2) in
          let t_assign = if r = "" then t2 else trm_set (trm_var r) t1' in
          if is_terminal
            then t_assign
            else begin 
                 incr nb_gotos;
                 trm_seq_nomarks [t_assign; trm_goto exit_label]
                 end
        | _ ->
            incr nb_gotos;
            trm_goto exit_label
        end
      | _ ->
          incr nb_gotos;
          trm_goto exit_label
      end
    | Trm_let_fun _ -> t (* do not recurse through local function definitions *)
    | _-> trm_map_with_terminal is_terminal aux t
  in
  let t = aux true t in
  (t, !nb_gotos)

(* [inline_aux index body_mark p_local t]: inline a function call, 
      [index] - index of the instruction containing the function call,
      [body_mark] - mark usef for the transflated body of the function,
      [p_local] - path from the instructions that contains the function call to the function call itself,
      [t] - ast of the sequence containing the function call. *)

(* LATER: inlining of f(3) could be ideally implemented as  variable.inline + function.beta,
   but for now we implement a function that covers both beta and inline at once, as it is simpler *)
let inline_aux (index : int) (body_mark : mark option) (p_local : path) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, trm_to_change, lback = Internal.get_trm_and_its_relatives index tl in
    let fun_call = Path.resolve_path p_local trm_to_change in
    begin match fun_call.desc with
    | Trm_apps(tfun, fun_call_args) ->
      let fun_decl = begin match tfun.desc with
      | Trm_var (_, f) ->
        begin match Internal.toplevel_decl ~require_body:true f with
        | Some decl -> decl
        | _ -> fail tfun.loc "Function_core.inline_aux: couldn't find the toplevel decl for the targeted function call"
        end
      | Trm_let_fun _ -> tfun
      | _ -> fail tfun.loc "Function_core.inline_aux: expected either a function call or a beta function call"
      end in
     begin match fun_decl.desc with
     | Trm_let_fun (_f, ty, args, body) ->
        let fun_decl_arg_vars = fst (List.split args) in
        let fresh_args = List.map Internal.fresh_args fun_call_args in
        let fun_decl_body = List.fold_left2 (fun acc x y -> Internal.subst_var x y acc) body fun_decl_arg_vars fresh_args in
        let fun_decl_body = List.fold_left2 (fun acc x y -> Internal.change_trm x y acc) fun_decl_body fresh_args fun_call_args in
        let name = match trm_to_change.desc with | Trm_let (vk, (x, _), _) -> x| _ -> ""  in
        let processed_body, nb_gotos = process_return_in_inlining "exit_body" name fun_decl_body in
        let marked_body = begin match body_mark with
        | Some b_m -> if b_m <> "" then trm_add_mark b_m processed_body  else Internal.set_nobrace_if_sequence processed_body
        | _ -> Internal.set_nobrace_if_sequence processed_body
        end  in
        let exit_label = if nb_gotos = 0 then trm_seq_no_brace [] else trm_labelled "exit_body" (trm_lit (Lit_unit)) in
        let inlined_body =
         if is_type_unit(ty)
           then [marked_body; exit_label]
           else
            [trm_pass_marks fun_call (trm_let_mut (name, ty) (trm_uninitialized ()));
                marked_body; exit_label]
           in
        let new_tl = Mlist.merge lfront (Mlist.of_list inlined_body) in
        let new_tl = Mlist.merge new_tl lback in
      trm_seq ~annot:t.annot new_tl
     | _ -> fail fun_decl.loc "Function_core.inline_aux: failed to find the top level declaration of the function"
     end
    | _ -> fail fun_call.loc "Function_core.inline_aux: expected a target to a function call"
    end
  | _ -> fail t.loc "Function_core.inline_aux: the targeted function call should be contained into an instuction that belongs to a local or global scope"

(* [inline index body_mark p_local t p]: applies [inline_aux] at the trm [t] with path [p]. *)
let inline (index: int) (body_mark : string option) (p_local : path) : Target.Transfo.local =
  Trace.time "Function_core.inline" (fun () -> Target.apply_on_path (
    Trace.time "Function_core.inline_aux" (fun () -> inline_aux index body_mark p_local)))

(* [use_infix_ops_aux allow_identity t]: transforms an explicit write operation to an implicit one
      [allow_identity] - if true then the transformation will never fail 
      [t] - ast of the write operation *)
let use_infix_ops_aux (allow_identity : bool) (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, [ls; rs]) when is_set_operation t ->
    begin match rs.desc with
    | Trm_apps (f1, [get_ls; arg]) ->
      begin match trm_prim_inv f1 with
      | Some p when is_infix_prim_fun p ->
        let aux s = AstC_to_c.ast_to_string s in
        if aux ls <> aux (get_operation_arg get_ls) && aux ls <> aux (get_operation_arg arg)
          then t
          else
            let binop = match get_binop_from_prim p with | Some binop -> binop | _ -> fail f.loc "Function_core.use_infix_ops_aux: this should never happen" in
            if not (aux ls = aux (get_operation_arg get_ls)) then trm_prim_compound ~annot:t.annot binop ls get_ls else  trm_prim_compound ~annot:t.annot binop ls arg
      | _ ->
        if allow_identity then t else
        fail f1.loc "Function_core.use_infix_ops_aux: expected a write operation of the form x = f(get(x), arg) or x = f(arg, get(x) where f is a binary operator that can be written in an infix form"

      end
    | _ -> if allow_identity then t else
           fail rs.loc "Function_core.use_infix_ops_aux: expeted a write operation of the form x = f(get(x), arg) or x = f(arg, get(x))"
    end
  | _-> if allow_identity then t else fail t.loc "Function_core.use_infix_ops_aux: expected an infix operation of the form x = f(x,a) or x = f(a,x)"

(* [use_infix_ops allow_identity t p]: applies [use_infix_ops_aux] at the trm [t] with path [p]. *)
let use_infix_ops (allow_identity: bool) : Target.Transfo.local =
  Target.apply_on_path (use_infix_ops_aux allow_identity)

(* [uninline_aux fct_decl t]: takes a function declaration [fct_decl], for example
   [void gtwice(int x) { g(x, x); }], and expects a term [t] that matches the body
   of the function, for example [g(3,3)]. It performs some matching to resolve [x]
   and returns the term [gtwice(3)], which is equivalent to [t] up to inlining. *)
let uninline_aux (fct_decl : trm) (t : trm) : trm =
  match fct_decl.desc with
  | Trm_let_fun (name, _rettype, targs, body) ->
      let inst = Trm_matching.rule_match ~higher_order_inst:true targs body t in
      let args = Ast.tmap_to_list (List.map fst targs) inst in
      trm_apps (trm_var name) args
  | _ -> fail fct_decl.loc "Function_core.uninline: fct argument should target a function definition"


(* [uninline fct_decl t p]: applies [uninline_aux] at the trm [t] with path [p]. *)
let uninline (fct_decl : trm) : Target.Transfo.local =
  Target.apply_on_path (uninline_aux fct_decl)

(* [rename_args_aux vl t]: renames arguments of function [t] and replace all the occurrences of its
    arguments of the args inside its body with the new names provided as arguments,
      [vl] - new arguments,
      [t] - ast of the function declaration whose arguments are going to be altered. *)
let rename_args_aux (vl : var list) (t : trm) : trm =
  match t.desc with
  | Trm_let_fun (f, retty, args, body) ->
    let renamed_args = List.map2 (fun v1 (arg1, ty1) -> if v1 <> "" then (v1, ty1) else (arg1, ty1)) vl args in
    let assoc_list = List.fold_left2 (fun acc v1 (arg1, _ty1) -> if v1 <> "" then (arg1, trm_var v1) ::  acc else acc) [] vl args in
    let tm = map_from_trm_var_assoc_list assoc_list in
    let new_body = Internal.subst tm body in
    trm_let_fun f retty renamed_args new_body
  | _ -> fail t.loc "Function_core.rename_args_aux: expected a target to a function declaration"

(* [rename_args vl t p]: apply [rename_aux] at trm [t] with path [p] *)
let rename_args (vl : var list) : Target.Transfo.local =
  Target.apply_on_path (rename_args_aux vl)



(* [replace_with_change_args_aux new_fun_name arg_mapper t]: change the name of the called function and its arguments
      [new_fun_name] - the new name that is going to replace the current one,
      [arg_mapper] - a function to change the arguments. *)       
let replace_with_change_args_aux (new_fun_name : string) (arg_mapper : trms -> trms) (t : trm) : trm = 
  match t.desc with 
  | Trm_apps (f, args) -> {t with desc = Trm_apps (trm_var new_fun_name, arg_mapper args)}
  | _ -> fail t.loc "Function_core.replace_with_change_args_aux: expected a target to a function call"


(* [replace_with_change_args new_fun_name arg_mapper t p]: applies [replace_with_change_args_aux] at trm [t] with path [p]. *)
let replace_with_change_args (new_fun_name : string) (arg_mapper : trms -> trms) : Target.Transfo.local =
  Target.apply_on_path (replace_with_change_args_aux new_fun_name arg_mapper)
