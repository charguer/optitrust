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
let inline_aux (index : int) (body_mark : string) (_top_ast : trm) (p_local : path) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, trm_to_change, lback = Internal.get_trm_and_its_relatives index tl in
    let fun_call = Path.resolve_path p_local trm_to_change  in
    begin match fun_call.desc with
    | Trm_apps (tfun, fun_call_args) ->
      let fun_decl =
      begin match tfun .desc with
      | Trm_var f ->
        begin match Internal.toplevel_decl f with
        | Some decl -> decl
        | None -> fail tfun.loc "inline_aux: couldn't find the toplevel_decl for the targeted function call"
        end
      | Trm_let_fun _ -> tfun
      | _ -> fail tfun.loc "inline_aux: expected either a function call or beta funtion call"
      end in
      let  fun_decl_type, fun_decl_args, fun_decl_body = begin match fun_decl.desc with
                   | Trm_let_fun (_f, ty, args,body) -> ty, args, body
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

    | _ -> fail fun_call.loc "inline_aux: couldn't resolve the name of the function, target does not resolve to a function call"
    end



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



(* TODO: this new_rule_match function superseeds that from the Rewrite_core module.
   This function should be moved into a separate file, called trm_matching.ml. *)
exception Rule_mismatch
let new_rule_match ~higher_order_inst(*:bool*) (vars : typed_vars) (pat : trm) (t : trm) : tmap =

  (* [inst] maps each pattern variable to a term and to a type;
     when pattern variables are not yet instantiated,
     they are bound to the special term trm_uninitialized. *)
     (* LATER: we may need one day to introduce another special term Trm_uninstantiated  *)
  let inst = ref (List.fold_left (fun acc (x,ty) -> Trm_map.add x (ty, Ast.trm_uninitialized()) acc) Trm_map.empty vars) in
  let is_var (x : var) : bool =
    Trm_map.mem x !inst in
  let find_var (x : var) (u : trm) : unit =
    match Trm_map.find_opt x !inst with
    | None -> failwith "failure in rule_match: called find_var without first checking is_var"
    | Some (ty,t0) ->
        if Ast.is_trm_uninitialized t0 then
          inst := Trm_map.add x (ty,u) !inst
        else if not (Internal.same_trm t0 u) then begin
          Tools.printf "Mismatch on variable '%s' already bound to '%s' which is not identical to '%s'" x (Ast_to_c.ast_to_string t0) (Ast_to_c.ast_to_string u);
          raise Rule_mismatch
        end
    in
  let get_binding (x : var) : (var * typ) option =
    match Trm_map.find_opt x !inst with
    | None -> None
    | Some (ty,t0) -> match t0.desc with
       | Trm_var y -> Some (y,ty)
       | _ -> None
    in
  let with_binding (ty : typ) (x : var) (y : var) (f : unit -> unit) : unit =
     inst := Trm_map.add x (ty, trm_var y) !inst;
     f();
     inst := Trm_map.remove x !inst;
    (* Note: it would be incorrect to simply restore the map to its value before the call to [f],
       because other variables than [x] may have been instantiated during the call to [f]. *)
     in


  let rec aux (t1 : trm) (t2 : trm) : unit =
    let mismatch ?(t1:trm=t1) ?(t2:trm=t2) () : unit =
      Tools.printf "Comparing %s with %s" (Ast_to_c.ast_to_string t1) (Ast_to_c.ast_to_string t2);
      raise Rule_mismatch
      in
    let aux_list (ts1 : trms) (ts2 : trms) : unit =
      List.iter2 aux ts1 ts2 in
    (* [aux_with_bindings] is a function for matching two lists of terms,
       making sure to extend the [inst] map to take into account the
       variable names that are bound locally; for example,
         [int a = 3; return a]  should match  [int b = 3; return b]
      thus we need to introduce a binding from [a] to [b] using [with_binding]. *)
    let rec aux_with_bindings (ts1 : trms) (ts2 : trms) : unit =
      match ts1, ts2 with
      | [], [] -> ()
      | ({ desc = Trm_let (vk1, (x1,t1), init1); _ } as dt1) :: tr1,
        ({ desc = Trm_let (vk2, (x2,t2), init2); _ } as dt2) :: tr2 ->
          if not (vk1 = vk2) then begin
            Tools.printf "Kind mismatch on trm_let\n";
            mismatch ~t1:dt1 ~t2:dt2 ()
          end;
          (* TODO: find out why this test is making the fbody unit test fail
           if not (same_types t1 t2) then begin
            Tools.printf "Type mismatch on trm_let\n";
            mismatch ~t1:dt1 ~t2:dt2 ()
          end; *)
          aux init1 init2;
          with_binding t1 x1 x2 (fun () -> aux_with_bindings tr1 tr2)
      (* LATER: add support for Trm_let_fun, to allow matching local function definitions. *)
      | t1 :: tr1, t2 :: tr2 ->
          aux t1 t2;
          aux_with_bindings tr1 tr2
      | _ -> mismatch() (* note: in general, this should have been tested earlier on by comparing lengths *)
      in

    match t1.desc, t2.desc with

    (* Case for treating a match against a pattern variable *)
    | Trm_var x, _ when is_var x -> find_var x t2

    (* Case for treating a match against a pattern such as [body(i)],
       where [body] is a pattern variable that corresponds to a function. *)
    | Trm_apps ({ desc = Trm_var x; _}, ts1), _ when higher_order_inst && is_var x ->
        let msg1 i ti = fail None (Printf.sprintf "rule_match: the %d-th argument of the higher-order function variable %s is not a variable. It is the term: %s" i x (Ast_to_text.ast_to_string ti)) in
        let xargs = List.mapi (fun i ti -> match ti.desc with
          | Trm_var x
          (* LATER: find out if it is really correct to igore the get operation here *)
          | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [{desc = Trm_var x; _}]) -> x
          | _ -> msg1 i ti) ts1 in
        let msg2 i = fail None (Printf.sprintf "rule_match: the %d-th argument of the higher-order function variable %s is not found in the instantiation map" i x) in
        let targs = List.mapi (fun i xi -> match get_binding xi with Some typed_yi -> typed_yi | None -> msg2 i) xargs in
        let body = t2 in
        let func = trm_let_fun x (typ_unit()) targs body in
        find_var x func
        (* LATER: it would be equivalent, but slightly nicer, to use the types coming from the function type associated with x,
           rather that to take the local types associated with the variables provided as arguments to x. *)

    | Trm_var x1, Trm_var x2 when x1 = x2 -> ()

    | Trm_val v1, Trm_val v2 when Internal.same_val v1 v2 -> ()

    | Trm_for (index1, start1, stop1, step1, body1),
      Trm_for (index2, start2, stop2, step2, body2) ->
        aux start1 start2;
        begin match stop1, stop2 with (* TODO: Trm_for should have dir and stop as argument, where dir does not carry a trm *)
        | DirUp stopt1, DirUp stopt2 -> aux stopt1 stopt2
        | DirUpEq stopt1, DirUpEq stopt2 -> aux stopt1 stopt2
        | DirDown stopt1, DirDown stopt2 -> aux stopt1 stopt2
        | DirDownEq stopt1, DirDownEq stopt2 -> aux stopt1 stopt2
        | _ -> mismatch()
        end;
        begin match step1, step2 with
        | Step stept1, Step stept2 -> aux stept1 stept2
        | _ -> if step1 <> step2 then mismatch()
        end;
        with_binding (typ_int()) index1 index2 (fun () -> aux body1 body2)

    | Trm_for_c (init1, cond1, step1, body1), Trm_for_c (init2, cond2, step2, body2) ->
        aux_with_bindings [init1; cond1; step1; body1] [init2; cond2; step2; body2]

    | Trm_seq tl1, Trm_seq tl2 ->
        if Mlist.length tl1 <> Mlist.length tl2 then mismatch();
        aux_with_bindings (Mlist.to_list tl1) (Mlist.to_list tl2)

    | Trm_apps (f1, ts1), Trm_apps (f2, ts2) ->
        aux f1 f2;
        aux_list ts1 ts2;

    | _ -> mismatch()
    in
  aux pat t;
  Trm_map.map (fun (_ty,t) -> t) !inst

(* [uninline_aux fct_decl t] takes a function declaration [fct_decl], for example
   [void gtwice(int x) { g(x, x); }], and expects a term [t] that matches the body
   of the function, for example [g(3,3)]. It performs some matching to resolve [x]
   and returns the term [gtwice(3)], which is equivalent to [t] up to inlining. *)
let uninline_aux (fct_decl : trm) (t : trm) : trm =
  match fct_decl.desc with
  | Trm_let_fun (name, _rettype, targs, body) ->
      let inst = new_rule_match ~higher_order_inst:true targs body t in
      let args = Ast.tmap_to_list (List.map fst targs) inst in
      trm_apps (trm_var name) args
  | _ -> fail fct_decl.loc "uninline: fct argument should target a function definition"

let uninline (fct_decl : trm) : Target.Transfo.local =
  Target.apply_on_path (uninline_aux fct_decl)

