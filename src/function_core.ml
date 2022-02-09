open Ast
open Path

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [bind_intro_aux index fresh_name const p_local t]: bind the variable [fresh_name] to the function_call
    params:
      [index]: index of the instruction containing the targeted function call
      [fresh_name]: name of the variable which going to be binded to the function call
      [const]: a flag for the mutability of the binded variable
      [p_local]: the local path from the instruction containing the targeted function call
        to the targeted function call
      [t]: ast of the sequence containing the targeted function call
    return:
      the updated sequence with the new generated binding
*)
let bind_intro_aux (my_mark : string) (index : int) (fresh_name : var) (const : bool) (p_local : path) (_path : path) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     let lfront, instr, lback = Internal.get_trm_and_its_relatives index tl in
     let function_call = Path.resolve_path p_local instr in
     let has_reference_type = if (Str.string_before fresh_name 1) = "&" then true else false in
     let fresh_name = if has_reference_type then (Str.string_after fresh_name 1) else fresh_name in
    
     let function_type = match function_call.typ with
     | Some typ -> typ
     | None -> typ_auto() in
     let decl_to_change = Internal.change_trm function_call (trm_var_possibly_mut ~const ~typ:(Some function_type) fresh_name) instr in 
    
     let function_call = if my_mark <> "" then trm_add_mark my_mark function_call else function_call in
     let decl_to_insert =
      if const
        then trm_let_immut (fresh_name, function_type) function_call
        else trm_let_mut (fresh_name, function_type) function_call
      in
     let new_tl = Mlist.merge lfront (Mlist.of_list ([decl_to_insert] @ [decl_to_change])) in
     let new_tl = Mlist.merge new_tl lback in
     let res = trm_seq ~annot:t.annot ~marks:t.marks new_tl in
     res
  | _ -> fail t.loc "bind_intro_aux: expected the surrounding sequence"

let bind_intro ?(my_mark : string =  "") (index : int) (fresh_name : var) (const : bool) (p_local : path) : Target.Transfo.local =
  Target.applyp_on_path (bind_intro_aux my_mark index fresh_name const p_local)

(* [replace_return exit_label r t] remove all the return statemns from the body of a function declaration.
      these return statements will be replaced either by set operations if the return statment are not terminal
       then an additional goto statement is added.
    params:
      [exit_label]: this label is generated only if the body contains non terminal return instructions
      [r]: the name of the variable replacing the return statement
      [t]: the ast of the body of the function
    returns:
      the updated ast of the body of the function with the replaced all return statements
*)
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
            then
              t_assign
            else
              begin
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

(* [inline_aux index body_mark top_ast p_local t] replace a function call with the traslated body of the function called
    params:
      [index]: index of the instruction containing the function call
      [body_mark]: body_mark used for the traslated body of the function
      [top_ast]: the main ast of the file, this is used to check if ome variable is defined before or not
      [p_local]: path from the instruction containing the function call to the call
      [t]: ast of the sequence containing the instruction with the function call
    returns:
      the updated ast of the surrounding sequence where the update is the inserted body translation of the function called
*)
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
        begin match Internal.toplevel_decl f with
        | Some decl -> decl
        | _ -> fail tfun.loc "inline_aux: couldn't find the toplevel decl for the targeted function call"
        end
      | Trm_let_fun _ -> tfun
      | _ -> fail tfun.loc "inline_aux: expected either a function call or a beta function call"
      end in
     begin match fun_decl.desc with
     | Trm_let_fun (_f, ty, args, body) ->
        let fun_decl_arg_vars = fst (List.split args) in
        let fresh_args = List.map Internal.fresh_args fun_call_args in
        let fun_decl_body = List.fold_left2 (fun acc x y -> Internal.subst_var x y acc) body fun_decl_arg_vars fresh_args in
        let fun_decl_body = List.fold_left2 (fun acc x y -> Internal.change_trm x y acc) fun_decl_body fresh_args fun_call_args in
        let name = match trm_to_change.desc with | Trm_let (_, (x, _), _) -> x | _ -> ""  in
        let processed_body, nb_gotos = process_return_in_inlining "exit_body" name fun_decl_body in
        let marked_body = begin match body_mark with
        | Some b_m -> if b_m <> "" then trm_add_mark b_m processed_body  else Internal.set_nobrace_if_sequence processed_body
        | _ -> Internal.set_nobrace_if_sequence processed_body
        end  in
        let exit_label = if nb_gotos = 0 then trm_seq_no_brace [] else trm_labelled "exit_body" (trm_lit (Lit_unit)) in
        let inlined_body =
         if is_type_unit(ty)
           then [marked_body; exit_label]
           else [trm_let_mut ~marks:fun_call.marks (name, ty) (trm_uninitialized ());
                marked_body; exit_label]
           in
        let new_tl = Mlist.merge lfront (Mlist.of_list inlined_body) in
        let new_tl = Mlist.merge new_tl lback in
      trm_seq ~annot:t.annot ~marks:t.marks new_tl
     | _ -> fail fun_decl.loc "inline_aux: failed to find the top level declaration of the function"
     end
    | _ -> fail fun_call.loc "inline_aux: expected a target to a function call"
    end
  | _ -> fail t.loc "inline_aux: the targeted function call should be contained into an instuction that belongs to a local or global scope"


let inline (index: int) (body_mark : string option) (p_local : path) : Target.Transfo.local =
  Target.apply_on_path (inline_aux index body_mark p_local)

(* [use_infix_ops_aux t] transforms a write operation into app and write operation in the case when
      the operator applied has the neccessary shape
    params:
      [t]: the ast of the write operation
    return:
      ast of a binary compound operation
*)

let use_infix_ops_aux (allow_identity : bool) (t : trm) : trm = 
  match t.desc with 
  | Trm_apps (f, [ls; rs]) when is_set_operation t ->
    begin match rs.desc with 
    | Trm_apps (f1, [get_ls; arg]) ->
      begin match trm_prim_inv f1 with 
      | Some p when is_infix_prim_fun p ->
        let aux s = Ast_to_c.ast_to_string s in 
        let final_trm =
        if aux ls = aux (get_operation_arg get_ls) then t else  trm_apps ~marks:t.marks f [ls; trm_apps f1 [arg; get_ls]] in
        trm_annot_add App_and_set final_trm

      | _ -> 
        if allow_identity then t else
        fail f1.loc "use_infix_ops_aux: expected a write operation of the form x = f(get(x), arg) or x = f(arg, get(x) where f is a binary operator that can be written in an infix form"
      
      end 
    | _ -> if allow_identity then t else 
           fail rs.loc "use_infix_ops_aux: expeted a write operation of the form x = f(get(x), arg) or x = f(arg, get(x))"
    end 
  | _-> if allow_identity then t else fail t.loc "use_infi_ops_aux: expected an infix operation of the form x = f(x,a) or x = f(a,x)"


let use_infix_ops (allow_identity: bool) : Target.Transfo.local =
  Target.apply_on_path (use_infix_ops_aux allow_identity)

(* [uninline_aux fct_decl t] takes a function declaration [fct_decl], for example
   [void gtwice(int x) { g(x, x); }], and expects a term [t] that matches the body
   of the function, for example [g(3,3)]. It performs some matching to resolve [x]
   and returns the term [gtwice(3)], which is equivalent to [t] up to inlining. *)
let uninline_aux (fct_decl : trm) (t : trm) : trm =
  match fct_decl.desc with
  | Trm_let_fun (name, _rettype, targs, body) ->
      let inst = Trm_matching.rule_match ~higher_order_inst:true targs body t in
      let args = Ast.tmap_to_list (List.map fst targs) inst in
      trm_apps (trm_var name) args
  | _ -> fail fct_decl.loc "uninline: fct argument should target a function definition"

let uninline (fct_decl : trm) : Target.Transfo.local =
  Target.apply_on_path (uninline_aux fct_decl)

(* [rename_args_aux vl t] rename arguments of funciton [t] and replace all the 
    occurrences of the args inside the body with the new names *)
let rename_args_aux (vl : var list) (t : trm) : trm = 
  match t.desc with 
  | Trm_let_fun (f, retty, args, body) ->
    let renamed_args = List.map2 (fun v1 (arg1, ty1) -> if v1 <> "" then (v1, ty1) else (arg1, ty1)) vl args in 
    let assoc_list = List.fold_left2 (fun acc v1 (arg1, _ty1) -> if v1 <> "" then (arg1, trm_var v1) ::  acc else acc) [] vl args in 
    let tm = map_from_trm_var_assoc_list assoc_list in 
    let new_body = Internal.subst tm body in 
    trm_let_fun f retty renamed_args new_body
  | _ -> fail t.loc "rename_args_aux: expected a target to a function declaration"

let rename_args (vl : var list) : Target.Transfo.local =
  Target.apply_on_path (rename_args_aux vl)

