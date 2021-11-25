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
     let function_call, _ = Path.resolve_path p_local instr in
     let has_reference_type = if (Str.string_before fresh_name 1) = "&" then true else false in
     let fresh_name = if has_reference_type then (Str.string_after fresh_name 1) else fresh_name in
     let decl_to_change = Internal.change_trm function_call (trm_var fresh_name) instr in
     let function_call = if my_mark <> "" then trm_add_mark my_mark function_call else function_call in
     let function_type = match function_call.typ with
     | Some typ -> typ
     (* Maybe it should fail here!! *)
     | None -> typ_auto() in
     let decl_to_insert =
      if const then
        trm_let Var_immutable (fresh_name, typ_const function_type) function_call
      else
        let ptrkind = if has_reference_type then Ptr_kind_ref else Ptr_kind_mut in
        trm_let Var_mutable (fresh_name, typ_ptr ~typ_attributes:[GeneratedStar] ptrkind (function_type)) (trm_apps  (trm_prim (Prim_new (function_type))) [function_call])
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
let process_return_in_inlining (exit_label : label) (r : var) (t : trm) : (trm * int ) =
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
(* TODO: let beta = inline *)
let inline_aux (index : int) (body_mark : string) (_top_ast : trm) (p_local : path) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, trm_to_change, lback = Internal.get_trm_and_its_relatives index tl in
    let fun_call = fst (Path.resolve_path p_local trm_to_change) in (* TODO: rename to resolve_path_and_ctx;   get_trm_at_path -> first projection of it *)

   (* TODO: let fun_decl =
       begin match fun_call.desc with
       | Trm_apps (tfun, args) ->
                   begin match tfun.desc with
                   | Trm_var f ->
                         begin match Internal.toplevel_decl fun_call_name with
                        | Some decl -> decl
                        | None -> fail t
                        end
                   | Trm_let_fun ... -> tfun
                   end
       | _ -> error
       in
    let fun_decl ... *)
    let fun_call_name, fun_call_args = begin match fun_call.desc with
                   | Trm_apps ({desc = Trm_var f; _}, args) -> f, args
                   | _ -> fail fun_call.loc "inline_aux: couldn't resolve the name of the function, target does not resolve to a function call"
                   end in
    let fun_decl = Internal.toplevel_decl fun_call_name in
    let fun_decl = begin match fun_decl with
      | Some decl -> decl
      | None -> fail t.loc "inline_aux: no trm in top level gives the declaration with the given name"
      end in

    let (* TODO: obtain f here as well *) fun_decl_type, fun_decl_args, fun_decl_body = begin match fun_decl.desc with
                   | Trm_let_fun (f, ty, args,body) (* TODO: remove when clause *) when f = fun_call_name  -> ty, args, body
                   | _ -> fail fun_decl.loc "inline_aux: failed to find the top level declaration of the function"
                   end in
   let fun_decl_arg_vars = fst (List.split fun_decl_args) in
   (* Since there is a chance that there can be arguments which have the same name both on the function call and function definition,
      a replacing of the current args with the function call args with an underscore prefix is needed *)
   let fresh_args = List.map Internal.fresh_args fun_call_args in

   let fun_decl_body = List.fold_left2 (fun acc x y -> Internal.subst_var x y acc) fun_decl_body fun_decl_arg_vars fresh_args in
   let fun_decl_body = List.fold_left2 (fun acc x y -> Internal.change_trm x y acc) fun_decl_body fresh_args fun_call_args in

   let name = match trm_to_change.desc with| Trm_let (_, (x, _), _) -> x | _ -> ""  in
   let processed_body, nb_gotos = process_return_in_inlining "exit_body" name fun_decl_body in
   let marked_body = trm_add_mark body_mark processed_body in
      (* if name = ""
        then trm_add_mark body_mark fun_decl_body
        else trm_add_mark body_mark processed_body
      in *)
   let exit_label = if nb_gotos = 0 then trm_seq_no_brace [] else trm_labelled "exit_body" (trm_lit (Lit_unit)) in
   let inlined_body =
    if is_type_unit(fun_decl_type)
      then [marked_body; exit_label]
      else  [trm_let ~marks:fun_call.marks Var_mutable (name, fun_decl_type) (trm_prim (Prim_new fun_decl_type));
              marked_body;exit_label]
      in

    let new_tl = Mlist.merge lfront (Mlist.of_list inlined_body) in
    let new_tl = Mlist.merge new_tl lback in
    trm_seq ~annot:t.annot ~marks:t.marks new_tl
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"


let inline (index: int) (body_mark : string) (top_ast : trm) (p_local : path) : Target.Transfo.local =
  Target.apply_on_path (inline_aux index body_mark top_ast p_local)

(* [use_infix_ops_aux t] transforms a write operation into app and write operation in the case when
      the operator applied has the neccessary shape
    params:
      [t]: the ast of the write operation
    return:
      the same ast node with the added annotation App_and_set
*)
let use_infix_ops_aux (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, [ls; rs]) when is_set_operation t ->
    begin match rs.desc with
    | Trm_apps (f1, [get_ls; arg])  ->
      begin match trm_prim_inv f1 with
      | Some p when is_infix_prim_fun p ->
        let final_trm =
        if Internal.same_trm ls get_ls then t else  trm_apps ~marks:t.marks f [ls; trm_apps f1 [arg; get_ls]] in
        trm_annot_add App_and_set final_trm
      | _ -> fail f1.loc "use_infix_ops_aux: expected a write operatoin of the form x = f(get(x), arg where f should be a binary operation which supports app and set operations"
      end
    | _ ->
      fail rs.loc "use_infix_ops: expected a write operation of the form x = f(get(x), arg)"
    end
  | _ -> fail t.loc "use_infix_ops: expected an infix operation of the form x = f(x,a)"

let use_infix_ops : Target.Transfo.local =
  Target.apply_on_path (use_infix_ops_aux)


(* [beta_aux t] replace a function call with another function call where the application is its complete body declaration
    params:
      [t]: ast of targeted function call
    returns:
      the updated ast of the surrounding sequence where the update is the inserted body translation of the function called
*)
let beta_aux (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, arg) ->
    begin match f.desc with
    | Trm_var f1 ->
      let fun_decl = Internal.toplevel_decl f1  in
      begin match fun_decl with
      | Some fu ->
        trm_apps fu arg
      | _ -> fail t.loc "beta_aux: couldn't find the toplevel declaration of the targeted function"
      end

    | _ -> fail t.loc "beta_aux: the target should be a user defined function call and not an internal one"
    end

  | _ -> fail t.loc" beta_aux: expected a target to the function call"


let beta : Target.Transfo.local =
  Target.apply_on_path (beta_aux)

