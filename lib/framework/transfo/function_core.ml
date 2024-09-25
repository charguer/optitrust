open Prelude
open Path
open Target

(** [bind_intro_at my_mark index fresh_name vonst p_local t]: bind the variable [fresh_name] to the targeted function call,
      [my_mark] - put a mark on the targeted function call,
      [index] - index of the instruction that contains the targeted function call on its surrouding sequence,
      [const] - flag for the mutability of the binded variable,
      [p_local] - path from the instruction containing the function call to the function call itself,
      [t] - ast of the sequence that contains the targeted function call. *)
let bind_intro_at (my_mark : string) (index : int) (fresh_name : string) (const : bool) (p_local : path) (t : trm) : trm =
  let error = "Function_core.bind_intro_at: expected the surrouding sequence of the targeted call" in
  let tl = trm_inv ~error trm_seq_inv t in

  let f_update (t : trm) : trm =
     let function_call = Path.resolve_path p_local t in
     let has_reference_type = if (Str.string_before fresh_name 1) = "&" then true else false in
     let fresh_name = if has_reference_type then (Str.string_after fresh_name 1) else fresh_name in
     let fresh_var = new_var fresh_name in

      let function_type = match function_call.typ with
      | Some typ -> typ
      |  None -> typ_auto in
      let change_with = (trm_var_possibly_get ~const ~typ:function_type fresh_var) in
      let decl_to_change = Internal.change_trm function_call change_with t in

      let function_call = trm_add_mark my_mark function_call in
      let decl_to_insert =
      if const
        then trm_let (fresh_var, function_type) function_call
        else trm_let_mut (fresh_var, function_type) function_call
      in
      trm_seq_nobrace_nomarks [decl_to_insert; decl_to_change]
    in

  let new_tl = Mlist.update_nth index f_update tl in
  trm_seq ~annot:t.annot new_tl


(** [inline_at index body_mark p_local t]: inline a function call,
      [index] - index of the instruction containing the function call,
      [body_mark] - mark usef for the transflated body of the function,
      [p_local] - path from the instructions that contains the function call to the function call itself,
      [t] - ast of the sequence containing the function call. *)

(* LATER: inlining of f(3) could be ideally implemented as  variable.inline + function.beta,
   but for now we implement a function that covers both beta and inline at once, as it is simpler *)
let inline_at (index : int) (body_mark : mark) (subst_mark : mark) (p_local : path) (t : trm) : trm =
  let error = "Function_core.inline_at: the targeted function call should be contained into an instruction that
     belongs toa local or global scope" in
  let tl = trm_inv ~error trm_seq_inv t in

  let f_update (t : trm) : trm =
    let fun_call = Path.resolve_path p_local t in
    begin match fun_call.desc with
    | Trm_apps (tfun, fun_call_args, fun_ghost_args) ->
      let fun_decl = begin match tfun.desc with
      | Trm_var f ->
        begin match Internal.toplevel_decl ~require_body:true f with
        | Some decl -> decl
        | _ -> trm_fail tfun (sprintf "Function_core.inline_at: couldn't find the toplevel decl for the targeted function call '%s'" (var_to_string f))
        end
      | Trm_let_fun _ -> tfun
      | _ -> trm_fail tfun "Function_core.inline_at: expected either a function call or a beta function call"
      end in
      begin match fun_decl.desc with
      | Trm_let_fun (_f, ret_ty, args, body, _) ->
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
          Trace.justif "inlining pure expressions is always correct"
        end;
        let fun_decl_body = trm_subst subst_map (trm_copy body) in
        let name = match t.desc with
          | Trm_let ((x, _), _) -> x
          | _ -> dummy_var
        in
        let processed_body, nb_gotos = Internal.replace_return_with_assign ~exit_label:"exit_body" (typ_ptr ret_ty) name fun_decl_body in
        if !Flags.check_validity && nb_gotos > 0 then
          trm_fail t "inlining functions featuring return instructions in the body is not yet supported";
        let processed_body = begin
          (* TODO: for now, remove top level admitteds during inlining, think about alternatives *)
          let instrs = trm_inv ~error:"expected sequence" trm_seq_inv processed_body in
          trm_like ~old:processed_body (trm_seq (Mlist.filter (fun t ->
            Option.is_none (Resource_trm.admitted_inv t)
          ) instrs))
        end in
        let marked_body = if body_mark <> "" then trm_add_mark body_mark processed_body else Nobrace.set_if_sequence processed_body in
        let exit_label = if nb_gotos = 0 then trm_seq_nobrace_nomarks [] else trm_add_label "exit_body" (trm_lit (Lit_unit)) in
        let inlined_body =
          if is_typ_unit ret_ty
            then [marked_body; exit_label]
            else
              [trm_pass_marks fun_call (trm_let_mut_uninit (name, ret_ty));marked_body; exit_label]
          in
        trm_seq_nobrace_nomarks inlined_body

      | _ -> trm_fail fun_decl "Function_core.inline_at: failed to find the top level declaration of the function"
      end
    | _ -> trm_fail fun_call "Function_core.inline_at: expected a target to a function call"
    end
    in
    let new_tl = Mlist.update_nth index f_update tl in
    trm_seq ~annot:t.annot new_tl

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
let uninline_on (fct_decl : trm)
 (to_type_ret_t : seq_component list option ref)
 (span : Dir.span) (t_seq : trm) : trm =
  update_span_helper span t_seq (fun instrs ->
  let (f, ret_typ, targs, body, spec) = Pattern.pattern_match fct_decl [
    Pattern.(trm_let_fun !__ !__ !__ !__ !__) (fun f ret_typ targs body spec () ->
      (f, ret_typ, targs, body, spec)
    );
    Pattern.__ (fun () ->
      trm_fail fct_decl "fct argument should target a function definition"
    )
  ] in
  let body_instrs = trm_inv ~error:"expected sequence" trm_seq_inv body in
  let ret_var = ref None in
  let ret_ptr_typ = typ_ptr ret_typ in
  (* 1. if last function instruction is return v, change to x = v with x fresh var. *)
  let ret_body_instrs = Pattern.pattern_match (Mlist.lst body_instrs) [
    Pattern.(some (trm_return !__)) (fun val_opt () ->
      let prev_instrs = Mlist.pop_back body_instrs in
      match val_opt with
      | None -> prev_instrs
      | Some v ->
        let rv = new_var "ret" in
        ret_var := Some rv;
        let replacement_instr = trm_set (trm_var ~typ:ret_ptr_typ rv) v in
        Mlist.push_back replacement_instr prev_instrs
    );
    Pattern.__ (fun () -> body_instrs);
  ] in
  (* 2. check that there is no return in the rest of the function.
        also delete admitted instructions. *)
  let rec check_body t =
    Pattern.pattern_match t [
      Pattern.(trm_return __) (fun () ->
        trm_fail t "function has unsupported return instruction"
      );
      Resource_trm.Pattern.(admitted __) (fun () ->
        trm_seq_nobrace_nomarks []
      );
      Pattern.__ (fun () -> trm_map check_body t)
    ]
  in
  let ret_body = Nobrace.remove_after_trm_op check_body (trm_seq ret_body_instrs) in
  (* 3. try matching patched function body with target code. *)
  let ret_targs =
    match !ret_var with
    | None -> targs
    | Some rv -> (rv, ret_ptr_typ) :: targs
  in
  let inst = Trm_matching.rule_match (*~higher_order_inst:true*) ret_targs ret_body (trm_seq instrs) in
  let ret_args = Trm.tmap_to_list (List.map fst ret_targs) inst in
  (* 4. check validity: instantiated arguments must be pure,
        and a separate resource must be owned on the eventual return variable  *)
  if !Flags.check_validity then begin
    let _f_contract = match spec with
    | FunSpecUnknown | FunSpecReverts _ -> failwith "expected function contract"
    | FunSpecContract c -> c
    in
    Var_map.iter (fun _ arg_val ->
      if not (Resources.trm_is_pure arg_val) then
        trm_fail arg_val "basic function uninlining does not support non-pure arguments, combine with variable binding and inline"
    ) inst;
    (* DEPRECATED: is it really dangerous to alias an argument resource with the return address resource?
    match !ret_var with
    | None -> ()
    | Some rv ->
      let f_dsp = new_var (f.name ^ "_dsp") in
      let ret_cell = Resource_formula.formula_cell_var ~typ:ret_ptr_typ rv in
      let contract = FunSpecContract (Resource_contract.push_fun_contract_clause Modifies (Resource_formula.new_anon_hyp (), ret_cell) f_contract) in
      let f_def = trm_let_fun ~contract f_dsp typ_unit ret_targs ret_body in
      let f_call = trm_apps (trm_var f_dsp) ret_args in
      to_type_ret_t := Some [Trm f_def; Trm f_call];
    *)
    Trace.justif "uninlining pure expressions is always correct"
  end;
  [Trm (match !ret_var with
  | None -> trm_apps ~typ:ret_typ (trm_var f) ret_args
  | Some rv -> trm_set (List.hd ret_args)
    (trm_apps ~typ:ret_typ (trm_var f) (List.tl ret_args))
  )]
  )

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
  let (f, retty, args, body) = trm_inv ~error trm_let_fun_inv t in
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

(** [dsp_def_at index arg func t]: changes the destination pasing style,
     [index] - index of the targeted function definition on its surrounding sequence,
     [arg] - the new argument to be added on the new function definition,
     [func] - name of the newly added function definition,
     [t] - ast of the original function definition. *)
let dsp_def_at (index : int) (arg : string) (func : string) (t : trm) : trm =
  let error = "Function_core.dsp_def_at: expected the surrounding sequence of the targeted function definition." in
  let tl = trm_inv ~error trm_seq_inv t in

  let arg = new_var arg in
  let f_update (t : trm) : trm =
    let error = "Function_core.dsp_def_at: expected a target to a function definition." in
    let (f, ret_ty, tvl, body) = trm_inv ~error trm_let_fun_inv t in
    let new_body, _ = Internal.replace_return_with_assign (typ_ptr ret_ty) arg body in
    let new_args = tvl @ [(arg, typ_ptr ret_ty)] in
    let new_fun = if func = "dsp" then f.name ^ "_dsp" else func in
    let new_fun_var = new_var new_fun in
    let new_fun_def = trm_let_fun ~annot:t.annot new_fun_var typ_unit new_args new_body in
    trm_seq_nobrace_nomarks [t; new_fun_def]
   in
  let new_tl = Mlist.update_nth index f_update tl in
  trm_seq ~annot:t.annot new_tl

(** [dsp_call_on dps t]: changes a write operation with lhs a function call to a function call,
    [dsp] - the name of the function call, possibly empty to use the default name
    [t] - ast of the write operation. *)
let dsp_call_on (dsp : string) (t : trm) : trm =
  match t.desc with
  | Trm_apps (_, [lhs; rhs], _) when is_set_operation t ->
    begin match rhs.desc with
    | Trm_apps ({desc = Trm_var f; _}, args, _) ->
        let dsp_name = if dsp = "" then f.name ^ "_dsp" else dsp in
        (* TODO: avoid using name_to_var, take var as arg. *)
        trm_apps (trm_var (name_to_var dsp_name)) (args @ [lhs])
    | _ -> trm_fail rhs "Function_core.dsp_call_on: expected a target to a function call."
    end
  | _ -> trm_fail t "Function_core.dsp_call_on: expected a target to a function call, whose parent is a write operation."


(** [get_prototype t]: returns the return type of the function and the types of all its arguments.*)
let get_prototype (t : trm) : (typ * typed_vars) option =
  match t.desc with
  | Trm_let_fun (f, ret_ty, args, body, _) ->
    Some (ret_ty, args)
  | _ -> None
