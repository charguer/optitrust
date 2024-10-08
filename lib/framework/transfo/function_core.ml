open Prelude
open Path
open Target

(** [replace_return_with_assign_goto exit_label r t]: removes all the return statements from the body of a function declaration,
      [exit_label] - generated only if [t] is there is a sequence that contains not terminal instructions,
      [t] - ast of the body of the function. *)
let replace_return_with_assign_goto ?(exit_label = "exit") ?(res_ptr_name = "res") (t : trm) : trm =
  Nobrace.remove_after_trm_op (fun t ->
    (* TODO: Replace those goto with sequence block exits *)
    let result_ptr = ref None in
    let rec aux (t : trm) : trm =
      match t.desc with
      | Trm_abort (Ret t1) ->
        begin match t1 with
        | Some t2 ->
          let t2 = aux t2 in
          let r = match !result_ptr with
            | None ->
              let r = new_var res_ptr_name in
              result_ptr := Some r;
              r
            | Some r when var_eq r dummy_var -> failwith "Mixing return with values and without values"
            | Some r -> r
          in
          trm_seq_nobrace_nomarks [trm_set (trm_var r) t2; trm_goto exit_label]
        | None ->
          begin match !result_ptr with
          | None -> result_ptr := Some dummy_var
          | Some r when var_eq r dummy_var -> ()
          | Some _ -> failwith "Mixing return with values and without values"
          end;
          trm_goto exit_label
        end
      | _ -> trm_map aux t
    in
    let t' = aux t in
    match !result_ptr with
    | None -> t
    | Some res_ptr ->
      let instrs, seq_res = trm_inv trm_seq_inv t' in
      let exit_label_instr = trm_add_label exit_label (trm_unit ()) in
      if var_eq res_ptr dummy_var then begin
        if Option.is_some seq_res then failwith "Return without values for a body with a result";
        trm_like ~old:t' (trm_seq (Mlist.push_back exit_label_instr instrs))
      end else begin
        let seq_res_var = Option.unsome_or_else seq_res (fun () -> failwith "Return with values for a body without result") in
        let res_typ = ref typ_unit in
        let instrs = Sequence_core.change_binding seq_res_var (fun ty expr ->
            res_typ := ty;
            trm_set (trm_var res_ptr) expr
          ) instrs
        in
        let res_typ = !res_typ in
        let decl_res_ptr = trm_let_mut_uninit (res_ptr, res_typ) in
        let get_res_ptr = trm_let (seq_res_var, res_typ) (trm_get (trm_var res_ptr)) in
        trm_like ~old:t' (trm_seq_helper ~result:seq_res_var [Trm decl_res_ptr; TrmMlist instrs; Trm exit_label_instr; Trm get_res_ptr])
      end
  ) t

(** [beta_reduce_on ?body_mark ?subst_mark t]: beta reduce a function application.
      [body_mark] - mark used for the translated body of the function,
      [subst_mark] - mark added on substituted parameters
      [t] - ast of the sequence containing the function call. *)
let beta_reduce_on ?(body_mark : mark = no_mark) ?(subst_mark : mark = no_mark) (t : trm) : trm =
  match t.desc with
  | Trm_apps (tfun, fun_call_args, fun_ghost_args) ->
    begin match tfun.desc with
    | Trm_fun (args, ret_ty, body, _) ->
      let fun_decl_arg_vars = fst (List.split args) in
      let subst_map = Var_map.of_seq (Seq.append
        (List.to_seq (List.map2 (fun dv cv -> (dv, (trm_add_mark subst_mark cv))) fun_decl_arg_vars fun_call_args))
        (Seq.map (fun (g, f) -> (g, trm_add_mark subst_mark f)) (List.to_seq fun_ghost_args)))
      in
      if !Flags.check_validity then begin
        Var_map.iter (fun _ arg_val ->
          if not (Resources.trm_is_pure arg_val) then
            trm_fail arg_val "basic function inlining does not support non-pure arguments, combine with variable binding and inline"
        ) subst_map;
        Trace.justif "inlining a function when all arguments are pure is always correct"
      end;
      let fun_decl_body = trm_subst subst_map (trm_copy body) in
      (* LATER: In presence of a goto, this generates an ugly varaible name (res) and label name (exit) while we should be able to handle user given names. *)
      let processed_body = replace_return_with_assign_goto fun_decl_body in
      let processed_body =
        (* TODO: for now, remove top level admitted during inlining, think about alternatives *)
        let instrs, result = trm_inv ~error:"expected sequence" trm_seq_inv processed_body in
        trm_like ~old:processed_body (trm_seq ?result (Mlist.filter (fun t -> Option.is_none (Resource_trm.admitted_inv t)) instrs))
      in
      trm_add_mark body_mark processed_body
    | _ -> trm_fail tfun "Function_core.beta_reduce: expected a lambda abstraction on the left of the application"
    end
  | _ -> trm_fail t "Function_core.beta_reduce: expected a target to a function call"

(** [inline_on ?body_mark ?subst_mark t]: inline and beta reduce a function application.
      [body_mark] - mark used for the translated body of the function,
      [subst_mark] - mark added on substituted parameters
      [t] - ast of the sequence containing the function call. *)
(* TODO: Replace by a form of unfold that works for any variable *)
let inline_on ?(body_mark = no_mark) ?(subst_mark = no_mark) (t: trm): trm =
  match t.desc with
  | Trm_apps (tfun, fun_call_args, fun_ghost_args) ->
    begin match tfun.desc with
    | Trm_var f ->
      begin match Internal.toplevel_decl ~require_body:true f with
      | Some decl ->
        let _, _, fn_def = trm_inv trm_let_inv decl in
        beta_reduce_on ~body_mark ~subst_mark (trm_apps fn_def fun_call_args ~ghost_args:fun_ghost_args)
      | _ -> trm_fail tfun (sprintf "Function_core.inline_on: couldn't find the toplevel decl for the targeted function call '%s'" (var_to_string f))
      end
    | Trm_fun _ -> beta_reduce_on ~body_mark ~subst_mark t
    | _ -> trm_fail tfun "Function_core.inline_on: expected either a function call or a beta-redex"
    end
  | _ -> trm_fail t "Function_core.inline_on: expected a target to a function call"

(** [use_infix_ops_on allow_identity t]: transforms an explicit write operation to an implicit one
      [allow_identity] - if true then the transformation will never fail
      [t] - ast of the write operation *)
let use_infix_ops_on (allow_identity : bool) (t : trm) : trm =
  let trm_fail_or_identity fail_t msg =
    if allow_identity then begin
      Trace.justif_always_correct ();
      t
    end else
      trm_fail fail_t msg
  in
  match t.desc with
  | Trm_apps (f, [ls; rs], _) when is_set_operation t ->
    begin match rs.desc with
    | Trm_apps (f1, [get_ls; arg], _) ->
      begin match trm_prim_inv f1 with
      | Some (_, p) when is_infix_prim_fun p ->
        let binop = match get_binop_from_prim p with
        | Some binop -> binop
        | _ -> trm_fail f "Function_core.use_infix_ops_on: this should never happen"
        in
        let check_validity () = if !Flags.check_validity then begin
          (* FIXME: duplicated code with variable inline / bind *)
          if Resources.trm_is_pure ls then
            (* Case 1: pure expression *)
            Trace.justif "address is a pure expression"
          else begin
            (* Case 2: duplicable expression *)
            Resources.assert_not_self_interfering ls;
            Trace.justif "address is duplicable"
          end
        end in
        let same_ls_get_ls = are_same_trm ls (get_operation_arg get_ls) in
        let same_ls_arg = are_same_trm ls (get_operation_arg arg) in
        begin match (same_ls_get_ls, same_ls_arg) with
        | false, false ->
          trm_fail_or_identity ls "addresses were not equal"
        | false, true ->
          check_validity ();
          trm_compound_assign ~annot:t.annot binop ls get_ls
        | true, false ->
          check_validity ();
          trm_compound_assign ~annot:t.annot binop ls arg
        | true, true -> failwith "this should not happen"
        end
      | _ ->
        trm_fail_or_identity f1 "Function_core.use_infix_ops_on: expected a write operation of the form x = f(get(x), arg) or x = f(arg, get(x) where f is a binary operator that can be written in an infix form"

      end
    | _ -> trm_fail_or_identity rs "Function_core.use_infix_ops_on: expeted a write operation of the form x = f(get(x), arg) or x = f(arg, get(x))"
    end
  | _-> trm_fail_or_identity t "Function_core.use_infix_ops_on: expected an infix operation of the form x = f(x,a) or x = f(a,x)"

(** [uninline_on fct_decl t]: takes a function declaration [fct_decl], for example
   [void gtwice(int x) { g(x, x); }], and expects a term [t] that matches the body
   of the function, for example [g(3,3)]. It performs some matching to resolve [x]
   and returns the term [gtwice(3)], which is equivalent to [t] up to inlining. *)
let uninline_on (fct_decl : trm) (t : trm) : trm =
  let error = "Function_core.uninline: fct argument should target a function definition" in
  let (f, typ, targs, body, _) = trm_inv ~error trm_let_fun_inv fct_decl in
  let inst = Trm_matching.rule_match ~higher_order_inst:true targs body t in
  if !Flags.check_validity then begin
    Var_map.iter (fun _ arg_val ->
      if not (Resources.trm_is_pure arg_val) then
        trm_fail arg_val "basic function uninlining does not support non-pure arguments, combine with variable binding and inline"
    ) inst;
    Trace.justif "uninlining pure expressions is always correct"
  end;
  let args = Trm.tmap_to_list (List.map fst targs) inst in
  trm_pass_labels t (trm_apps (trm_var ~typ f) args)

(** [trm_var_assoc_list to_map al]: creates a map from an association list wher keys are variables and values are trms *)
let map_from_trm_var_assoc_list (al : (var * trm) list) : tmap =
  let tm = Var_map.empty in
  List.fold_left (fun acc (k, v) -> Var_map.add k v acc) tm al

(** [rename_args_on vl t]: renames arguments of function [t] and replace all the occurrences of its
    arguments of the args inside its body with the new names provided as arguments,
      [vl] - new arguments, can be [dummy_var] to avoid renaming.
      [t] - ast of the function declaration whose arguments are going to be altered. *)
let rename_args_on (vl : var list) (t : trm) : trm =
  let error = "Function_core.rename_args_on: expected a target to a function declaration" in
  let (f, retty, args, body, _) = trm_inv ~error trm_let_fun_inv t in
  let renamed_args = List.map2 (fun v1 (arg1, ty1) -> if v1 <> dummy_var then (v1, ty1) else (arg1, ty1)) vl args in
  let assoc_list = List.fold_left2 (fun acc v1 (arg1, ty1) -> if v1 <> dummy_var then (arg1, trm_var ~typ:ty1 v1) ::  acc else acc) [] vl args in
  let tm = map_from_trm_var_assoc_list assoc_list in
  let new_body = trm_subst tm body in
  trm_let_fun f retty renamed_args new_body

(** [replace_with_change_args_on new_fun_name arg_mapper t]: change the name of the called function and its arguments
      [new_fun_name] - the new name that is going to replace the current one,
      [arg_mapper] - a function to change the arguments. *)
let replace_with_change_args_on (new_fun_name : var) (arg_mapper : trms -> trms) (t : trm) : trm =
  let error = "Function_core.replace_with_change_args_on: expected a target to a function call" in
  let (f, args) = trm_inv ~error trm_apps_inv t in
  (* to change name and keep namespaces/id:
  let fv = trm_inv ~error trm_var_inv f in
  { namespaces = fv.namespaces; name = new_fun_name; id = fv.id } *)
  trm_replace (Trm_apps ((trm_var new_fun_name), arg_mapper args, [])) t

(** [dps_def_at index arg func t]: changes the destination pasing style,
     [index] - index of the targeted function definition on its surrounding sequence,
     [arg_name] - the new argument to be added on the new function definition,
     [fn_name] - name of the newly added function definition,
     [t] - ast of the original function definition. *)
let dps_def_at (index : int) (arg_name : string) ?(fn_name : string = "") (t : trm) : trm =
  let error = "Function_core.dps_def_at: expected the surrounding sequence of the targeted function definition." in
  let tl, result = trm_inv ~error trm_seq_inv t in
  let arg = new_var arg_name in
  let f_update (t : trm) : trm =
    let error = "Function_core.dps_def_at: expected a target to a function definition." in
    let (f, ret_ty, tvl, body, _) = trm_inv ~error trm_let_fun_inv t in
    let instrs, result = trm_inv ~error:"expected a sequence as the function body." trm_seq_inv body in
    let rec dps_return (t : trm) : trm =
      match t.desc with
      | Trm_abort (Ret (Some r)) ->
        let r = dps_return r in
        trm_seq_nobrace_nomarks [trm_set (trm_var arg) r; trm_ret None]
      | Trm_abort (Ret None) ->
        failwith "Function_core.dps_def_at: expect all the return to have a value"
      | _ ->
        trm_map dps_return t
    in
    let new_instrs = Mlist.map dps_return instrs in
    let rec dps_result (t: trm) : trm option =
      let open Option.Monad in
      match t.desc with
      | Trm_if (tcond, tthen, telse) ->
        let* tthen = dps_result tthen in
        let* telse = dps_result telse in
        Some (trm_like ~old:t (trm_if tcond tthen telse))
      | Trm_seq (instrs, Some seqres) ->
        Some (trm_like ~old:t (trm_seq (Sequence_core.change_binding seqres (fun _ expr ->
          match dps_result expr with
          | None -> trm_set (trm_var arg) expr
          | Some t -> t
        ) instrs)))
      | _ -> None
    in
    let new_body = match dps_result (trm_seq new_instrs ?result) with
      | None -> trm_fail t "Function_core.dps_def_at: the function body should have a result"
      | Some new_body -> new_body
    in
    let new_args = tvl @ [(arg, typ_ptr ret_ty)] in
    let new_fun_name = if fn_name = "" then f.name ^ "_dps" else fn_name in
    let new_fun_var = new_var new_fun_name in
    let new_fun_def = trm_let_fun ~annot:t.annot new_fun_var typ_unit new_args new_body in
    trm_seq_nobrace_nomarks [t; new_fun_def]
  in
  let new_tl = Mlist.update_nth index f_update tl in
  trm_seq ~annot:t.annot ?result new_tl

(** [dps_call_on dps t]: changes a write operation with lhs a function call to a function call,
    [dps] - the name of the function call, possibly empty to use the default name
    [t] - ast of the write operation. *)
let dps_call_on (dps : string) (t : trm) : trm =
  match t.desc with
  | Trm_apps (_, [lhs; rhs], _) when is_set_operation t ->
    begin match rhs.desc with
    | Trm_apps ({desc = Trm_var f; _}, args, _) ->
        let dps_name = if dps = "" then f.name ^ "_dps" else dps in
        (* TODO: avoid using name_to_var, take var as arg. *)
        trm_apps (trm_var (name_to_var dps_name)) (args @ [lhs])
    | _ -> trm_fail rhs "Function_core.dps_call_on: expected a target to a function call."
    end
  | _ -> trm_fail t "Function_core.dps_call_on: expected a target to a function call, whose parent is a write operation."


(** [get_prototype t]: returns the return type of the function and the types of all its arguments.*)
let get_prototype (t : trm) : (typ * typed_vars) option =
  match trm_let_fun_inv t with
  | Some (f, ret_ty, args, body, _) ->
    Some (ret_ty, args)
  | _ -> None
