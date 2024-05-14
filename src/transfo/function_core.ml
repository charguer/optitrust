open Prelude
open Path
open Target

(* [bind_intro_aux my_mark index fresh_name vonst p_local t]: bind the variable [fresh_name] to the targeted function call,
      [my_mark] - put a mark on the targeted function call,
      [index] - index of the instruction that contains the targeted function call on its surrouding sequence,
      [const] - flag for the mutability of the binded variable,
      [p_local] - path from the instruction containing the function call to the function call itself,
      [t] - ast of the sequence that contains the targeted function call. *)
let bind_intro_aux (my_mark : string) (index : int) (fresh_name : string) (const : bool) (p_local : path) (t : trm) : trm =
  let error = "Function_core.bind_intro_aux: expected the surrouding sequence of the targeted call" in
  let tl = trm_inv ~error trm_seq_inv t in

  let f_update (t : trm) : trm =
     let function_call = Path.resolve_path p_local t in
     let has_reference_type = if (Str.string_before fresh_name 1) = "&" then true else false in
     let fresh_name = if has_reference_type then (Str.string_after fresh_name 1) else fresh_name in
     let fresh_var = new_var fresh_name in

      let function_type = match function_call.typ with
      | Some typ -> typ
      |  None -> typ_auto() in
      let change_with = (trm_var_possibly_mut ~const ~typ:function_type fresh_var) in
      let decl_to_change = Internal.change_trm function_call change_with t in

      let function_call = trm_add_mark my_mark function_call in
      let decl_to_insert =
      if const
        then trm_let_immut (fresh_var, function_type) function_call
        else trm_let_mut (fresh_var, function_type) function_call
      in
      trm_seq_nobrace_nomarks [decl_to_insert; decl_to_change]
    in

  let new_tl = Mlist.update_nth index f_update tl in
  trm_seq ~annot:t.annot new_tl


(* [bind_intro ~my_mark index fresh_name const p_local]: applies [bind_intro_aux] at the trm [t] with path [p]. *)
let bind_intro ?(my_mark : string =  "") (index : int) (fresh_name : string) (const : bool) (p_local : path) : Transfo.local =
  apply_on_path (bind_intro_aux my_mark index fresh_name const p_local)


(* [inline_aux index body_mark p_local t]: inline a function call,
      [index] - index of the instruction containing the function call,
      [body_mark] - mark usef for the transflated body of the function,
      [p_local] - path from the instructions that contains the function call to the function call itself,
      [t] - ast of the sequence containing the function call. *)

(* LATER: inlining of f(3) could be ideally implemented as  variable.inline + function.beta,
   but for now we implement a function that covers both beta and inline at once, as it is simpler *)
let inline_aux (index : int) (body_mark : mark) (subst_mark : mark) (p_local : path) (t : trm) : trm =
  let error = "Function_core.inline_aux: the targeted function call should be contained into an instruction that
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
        | _ -> trm_fail tfun (sprintf "Function_core.inline_aux: couldn't find the toplevel decl for the targeted function call '%s'" (var_to_string f))
        end
      | Trm_let_fun _ -> tfun
      | _ -> trm_fail tfun "Function_core.inline_aux: expected either a function call or a beta function call"
      end in
      begin match fun_decl.desc with
      | Trm_let_fun (_f, ty, args, body, _) ->
        let fun_decl_arg_vars = fst (List.split args) in
        let subst_map = Var_map.of_seq (Seq.append
          (List.to_seq (List.map2 (fun dv cv -> (dv, (trm_add_mark subst_mark cv))) fun_decl_arg_vars fun_call_args))
          (Seq.map (fun (g, f) -> (g, trm_add_mark subst_mark f)) (List.to_seq fun_ghost_args)))
        in
        if !Flags.check_validity then begin
          (* FIXME: this check should be done by Variable.inline instead of this transfo, which should bind temporary variables *)
          Var_map.iter (fun _ arg_val ->
            if not (Resources.trm_is_pure arg_val) then
              trm_fail arg_val "inlining non-pure argument is not yet supported, requires checking for interference similar to instr.swap, loop.move_out, etc"
          ) subst_map;
          Trace.justif "inlining pure expressions is always correct"
        end;
        let fun_decl_body = trm_subst subst_map (trm_copy body) in
        let name = match t.desc with
          | Trm_let ((x, _), _) -> x
          | _ -> dummy_var
        in
        let processed_body, nb_gotos = Internal.replace_return_with_assign ~exit_label:"exit_body" name fun_decl_body in
        if !Flags.check_validity && nb_gotos > 0 then
          trm_fail t "inlining functions featuring return instructions in the body is not yet supported";
        let marked_body = if body_mark <> "" then trm_add_mark body_mark processed_body else Nobrace.set_if_sequence processed_body in
        let exit_label = if nb_gotos = 0 then trm_seq_nobrace_nomarks [] else trm_add_label "exit_body" (trm_lit (Lit_unit)) in
        let inlined_body =
          if is_type_unit(ty)
            then [marked_body; exit_label]
            else
              [trm_pass_marks fun_call (trm_let_mut (name, ty) (trm_uninitialized ()));marked_body; exit_label]
          in
        trm_seq_nobrace_nomarks inlined_body

      | _ -> trm_fail fun_decl "Function_core.inline_aux: failed to find the top level declaration of the function"
      end
    | _ -> trm_fail fun_call "Function_core.inline_aux: expected a target to a function call"
    end
    in
    let new_tl = Mlist.update_nth index f_update tl in
    trm_seq ~annot:t.annot new_tl

(* [inline index body_mark p_local t p]: applies [inline_aux] at the trm [t] with path [p]. *)
let inline (index: int) (body_mark : mark) ~(subst_mark : mark) (p_local : path) : Transfo.local =
  Stats.comp_stats "Function_core.inline" (fun () -> apply_on_path (
    Stats.comp_stats "Function_core.inline_aux" (fun () -> inline_aux index body_mark subst_mark p_local)))

(* [use_infix_ops_aux allow_identity t]: transforms an explicit write operation to an implicit one
      [allow_identity] - if true then the transformation will never fail
      [t] - ast of the write operation *)
let use_infix_ops_aux (allow_identity : bool) (t : trm) : trm =
  match t.desc with
  | Trm_apps (f, [ls; rs], _) when is_set_operation t ->
    begin match rs.desc with
    | Trm_apps (f1, [get_ls; arg], _) ->
      begin match trm_prim_inv f1 with
      | Some p when is_infix_prim_fun p ->
        let aux s = AstC_to_c.ast_to_string s in
        if aux ls <> aux (get_operation_arg get_ls) && aux ls <> aux (get_operation_arg arg)
          then t
          else
            let binop = match get_binop_from_prim p with | Some binop -> binop | _ -> trm_fail f "Function_core.use_infix_ops_aux: this should never happen" in
            if not (aux ls = aux (get_operation_arg get_ls)) then trm_prim_compound ~annot:t.annot binop ls get_ls else  trm_prim_compound ~annot:t.annot binop ls arg
      | _ ->
        if allow_identity then t else
        trm_fail f1 "Function_core.use_infix_ops_aux: expected a write operation of the form x = f(get(x), arg) or x = f(arg, get(x) where f is a binary operator that can be written in an infix form"

      end
    | _ -> if allow_identity then t else
           trm_fail rs "Function_core.use_infix_ops_aux: expeted a write operation of the form x = f(get(x), arg) or x = f(arg, get(x))"
    end
  | _-> if allow_identity then t else trm_fail t "Function_core.use_infix_ops_aux: expected an infix operation of the form x = f(x,a) or x = f(a,x)"

(* [use_infix_ops allow_identity t p]: applies [use_infix_ops_aux] at the trm [t] with path [p]. *)
let use_infix_ops (allow_identity: bool) : Transfo.local =
  apply_on_path (use_infix_ops_aux allow_identity)

(* [uninline_aux fct_decl t]: takes a function declaration [fct_decl], for example
   [void gtwice(int x) { g(x, x); }], and expects a term [t] that matches the body
   of the function, for example [g(3,3)]. It performs some matching to resolve [x]
   and returns the term [gtwice(3)], which is equivalent to [t] up to inlining. *)
let uninline_aux (fct_decl : trm) (t : trm) : trm =
  let error = "Function_core.uninline: fct argument should target a function definition" in
  let (f, _, targs, body) = trm_inv ~error trm_let_fun_inv fct_decl in
  let inst = Trm_matching.rule_match ~higher_order_inst:true targs body t in
  let args = Trm.tmap_to_list (List.map fst targs) inst in
  trm_pass_labels t (trm_apps (trm_var f) args)

(* [uninline fct_decl t p]: applies [uninline_aux] at the trm [t] with path [p]. *)
let uninline (fct_decl : trm) : Transfo.local =
  apply_on_path (uninline_aux fct_decl)

(* [trm_var_assoc_list to_map al]: creates a map from an association list wher keys are variables and values are trms *)
let map_from_trm_var_assoc_list (al : (var * trm) list) : tmap =
  let tm = Var_map.empty in
  List.fold_left (fun acc (k, v) -> Var_map.add k v acc) tm al

(* [rename_args_aux vl t]: renames arguments of function [t] and replace all the occurrences of its
    arguments of the args inside its body with the new names provided as arguments,
      [vl] - new arguments, can be [dummy_var] to avoid renaming.
      [t] - ast of the function declaration whose arguments are going to be altered. *)
let rename_args_aux (vl : var list) (t : trm) : trm =
  let error = "Function_core.rename_args_aux: expected a target to a function declaration" in
  let (f, retty, args, body) = trm_inv ~error trm_let_fun_inv t in
  let renamed_args = List.map2 (fun v1 (arg1, ty1) -> if v1 <> dummy_var then (v1, ty1) else (arg1, ty1)) vl args in
  let assoc_list = List.fold_left2 (fun acc v1 (arg1, _ty1) -> if v1 <> dummy_var then (arg1, trm_var v1) ::  acc else acc) [] vl args in
  let tm = map_from_trm_var_assoc_list assoc_list in
  let new_body = trm_subst tm body in
  trm_let_fun f retty renamed_args new_body

(* [rename_args vl t p]: applies [rename_aux] at trm [t] with path [p] *)
let rename_args (vl : var list) : Transfo.local =
  apply_on_path (rename_args_aux vl)

(* [replace_with_change_args_aux new_fun_name arg_mapper t]: change the name of the called function and its arguments
      [new_fun_name] - the new name that is going to replace the current one,
      [arg_mapper] - a function to change the arguments. *)
let replace_with_change_args_aux (new_fun_name : var) (arg_mapper : trms -> trms) (t : trm) : trm =
  let error = "Function_core.replace_with_change_args_aux: expected a target to a function call" in
  let (f, args) = trm_inv ~error trm_apps_inv t in
  (* to change name and keep qualifier/id:
  let fv = trm_inv ~error trm_var_inv f in
  { qualifier = fv.qualifier; name = new_fun_name; id = fv.id } *)
  trm_replace (Trm_apps ((trm_var new_fun_name), arg_mapper args, [])) t

(* [replace_with_change_args new_fun_name arg_mapper t p]: applies [replace_with_change_args_aux] at trm [t] with path [p]. *)
let replace_with_change_args (new_fun_name : var) (arg_mapper : trms -> trms) : Transfo.local =
  apply_on_path (replace_with_change_args_aux new_fun_name arg_mapper)

(* [dsp_def_aux index arg func t]: changes the destination pasing style,
     [index] - index of the targeted function definition on its surrounding sequence,
     [arg] - the new argument to be added on the new function definition,
     [func] - name of the newly added function definition,
     [t] - ast of the original function definition. *)

let dsp_def_aux (index : int) (arg : string) (func : string) (t : trm) : trm =
  let error = "Function_core.dsp_def_aux: expected the surrounding sequence of the targeted function definition." in
  let tl = trm_inv ~error trm_seq_inv t in

  let arg = Trm.new_var arg in
  let f_update (t : trm) : trm =
    let error = "Function_core.dsp_def_aux: expected a target to a function definition." in
    let (f, ret_ty, tvl, body) = trm_inv ~error trm_let_fun_inv t in
    let new_body, _ = Internal.replace_return_with_assign arg body in
    let new_args = tvl @ [(arg, typ_ptr Ptr_kind_mut ret_ty)] in
    let new_fun = if func = "dsp" then f.name ^ "_dsp" else func in
    let new_fun_var = new_var new_fun in
    let new_fun_def = trm_let_fun ~annot:t.annot new_fun_var (typ_unit ()) new_args new_body in
    trm_seq_nobrace_nomarks [t; new_fun_def]
   in
  let new_tl = Mlist.update_nth index f_update tl in
  trm_seq ~annot:t.annot new_tl

(* [dsp_def index arg func t p]: applies [dsp_def_aux] at trm [t] with path [p]. *)
  let dsp_def (index : int) (arg : string) (func : string) : Transfo.local =
  apply_on_path (dsp_def_aux index arg func)

(* [dsp_call_aux dps t]: changes a write operation with lhs a function call to a function call,
    [dsp] - the name of the function call, possibly empty to use the default name
    [t] - ast of the write operation. *)
let dsp_call_aux (dsp : string) (t : trm) : trm =
  match t.desc with
  | Trm_apps (_, [lhs; rhs], _) when is_set_operation t ->
    begin match rhs.desc with
    | Trm_apps ({desc = Trm_var f; _}, args, _) ->
        let dsp_name = if dsp = "" then f.name ^ "_dsp" else dsp in
        (* TODO: avoid using name_to_var, take var as arg. *)
        trm_apps (trm_var (name_to_var dsp_name)) (args @ [lhs])
    | _ -> trm_fail rhs "Function_core.dsp_call_aux: expected a target to a function call."
    end
  | _ -> trm_fail t "Function_core.dsp_call_aux: expected a target to a function call, whose parent is a write operation."

(* [dsp_call dsp t p]: applies [dsp_call_aux] at trm [t] with path [p]. *)
let dsp_call (dsp : string) : Transfo.local =
  apply_on_path (dsp_call_aux dsp)


(* [get_prototype t]: returns the return type of the function and the types of all its arguments.*)
let get_prototype (t : trm) : (typ * typed_vars) option =
  match t.desc with
  | Trm_let_fun (f, ret_ty, args, body, _) ->
    Some (ret_ty, args)
  | _ -> None
