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
      begin match Internal.toplevel_decl f with
      | Some decl ->
        let _, _, fn_def = trm_inv trm_let_inv decl in
        beta_reduce_on ~body_mark ~subst_mark (trm_apps fn_def fun_call_args ~ghost_args:fun_ghost_args)
      | _ -> trm_fail tfun (sprintf "Function_core.inline_on: couldn't find the toplevel decl for the targeted function call '%s'" (var_to_string f))
      end
    | Trm_fun _ -> beta_reduce_on ~body_mark ~subst_mark t
    | _ -> trm_fail tfun "Function_core.inline_on: expected either a function call or a beta-redex"
    end
  | _ -> trm_fail t "Function_core.inline_on: expected a target to a function call"


exception Use_infix_ops_on_failure of string

(** [use_infix_ops_on allow_identity t]: transforms an explicit write operation to an implicit one
      [allow_identity] - if true then the transformation will never fail
      [t] - ast of the write operation *)
    (* TODO: generalize to handle the cleanup of operations that are already infix,
       for example x += -4   to convert to  x -= 4. *)
let use_infix_ops_on (allow_identity : bool) (t : trm) : trm =
  let fail : 'a. string -> 'a = fun (msg : string) ->
    raise (Use_infix_ops_on_failure msg) in
  try
    Arith_core.(
    match t.desc with
    | Trm_apps (f, [ls; rs], _) when is_set_operation t -> (* LATER: use a proper inversor? *)
      (* t  =  set(ls, rs) *)
      (* Let's define the check that a given subterm is a [get(ls)] *)
      let is_get_of_ls (ti:trm) : bool =
        Option.value ~default:false (
          Option.map (are_same_trm ls) (trm_get_inv ti)
        ) in
      (* Let's reify the right-hand side as a arith-core AST *)
      let expr, atoms = trm_to_expr ~normalized:true rs in
      (* Let's define the operation that finds it an list of weighted
        expressions an atom of the form [get(ls)] with factor 1;
        if so, we remove it from the list. Could fail *)
      let rec remove_one_get_ls (wes:wexprs) : wexprs =
        match wes with
        | ((1,{expr_desc=Expr_atom (id,purity)}) as we)::wes' ->
              begin match Atom_map.find_opt id atoms with
              | Some (ti,_purity) ->
                  if is_get_of_ls ti then begin
                    (* found the [get(ls)], check duplicatability, then remove the item from the list *)
                    if !Flags.check_validity then begin
                      if not purity.redundant
                        then fail "Unable to introduce an infix op, because the LHS is not a duplicatable expressions.";
                      Trace.justif "the expression denoting the address is redundant.";
                      wes'
                    end else begin
                      wes' (* validity not checked *)
                    end
                  end else begin
                    (* else search further *)
                    we::(remove_one_get_ls wes')
                  end
              | None -> Tools.warn "expr_to_string: atom occurrence without entry in atom map---broken invariant."; fail "Internal error in use_infix_ops"
              end
        | we::wes' -> we::(remove_one_get_ls wes') (* else search further *)
        | [] -> (* [get(ls)] was not found in the list *)
            fail "Unable to introduce an infix op, because no occurence of the LHS in the RHS with unit weight"
        in
      (* We search in both sums (for [+=]) and products (for [*=]). *)
      let expr2, binop =
        match expr.expr_desc with
        | Expr_sum wes -> { expr with expr_desc = Expr_sum (remove_one_get_ls wes)}, Binop_add
        | Expr_prod wes -> { expr with expr_desc = Expr_prod (remove_one_get_ls wes)}, Binop_mul
        | _ -> fail ("Unable to introduce an infix op, because not a product or a sum on the RHS: "
                    ^ expr_to_string atoms expr)
        in
      (* Change [x += -a] into [x -= a] in case [a] is a single subexpression.
         Change [x += -3] into [x -= 3].
         Change [x *= 1/a] into [x /= a].  (only for exact_div, not integer division)
         Do not change [x *= 1/3] into [x /= 0.333333] due to harm of readability / loss of precision. *)
      let expr2, binop =
        (* for debug: Tools.warn "%s\n" (expr_to_string atoms expr2); *)
        match binop, expr2.expr_desc with
        | Binop_add, Expr_sum [(w, ({expr_desc = Expr_int 1; _} as eone))] when w < 0 ->
          { expr2 with expr_desc = Expr_sum [(-w,eone)] }, Binop_sub
        | Binop_add, Expr_int a when a < 0 -> { expr2 with expr_desc = Expr_int (- a) }, Binop_sub
        | Binop_add, Expr_float a when a < 0. -> { expr2 with expr_desc = Expr_float (-. a) }, Binop_sub
        | Binop_add, Expr_sum [(-1,e)] -> { expr2 with expr_desc = Expr_sum [(1,e)]}, Binop_sub
        | Binop_mul, Expr_prod [(-1,e)] -> { expr2 with expr_desc = Expr_sum [(1,e)]}, Binop_exact_div
        | _ -> expr2, binop
        in
      (* Finally we build the infix operation *)
      let rs2 = expr_to_trm atoms expr2 in
      let res = trm_compound_assign ~annot:t.annot ?typ:rs.typ binop ls rs2 in
      res
    | _-> fail "use_infix_ops_on: expected a set operation to be targeted"
    )
  with Use_infix_ops_on_failure msg ->
    if allow_identity then begin
      Trace.justif_always_correct ();
      t
    end else
      trm_fail t msg

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
  let (body_instrs, body_ret) = trm_inv ~error:"expected sequence" trm_seq_inv body in
  let ret_var = ref None in
  let ret_ptr_typ = typ_ptr ret_typ in
  (* 1. if last function instruction is return v (a.k.a const body_ret = v),
     change to x = v with x fresh var. *)
  let ret_body_instrs =
    match body_ret with
    | Some body_ret_var -> begin Pattern.pattern_match (Mlist.last body_instrs) [
      Pattern.(some (trm_let (var_eq body_ret_var) __ !__)) (fun v () ->
        let prev_instrs = Mlist.pop_back body_instrs in
        let rv = new_var "ret" in
        ret_var := Some rv;
        let replacement_instr = trm_set (trm_var ~typ:ret_ptr_typ rv) v in
        Mlist.push_back replacement_instr prev_instrs
      );
      Pattern.(some !__) (fun last_trm ->
        trm_fail last_trm "expected result variable initialization, found this"
      );
      Pattern.__ (fun () ->
        failwith "expected result variable initialization, found empty body"
      );
    ] end
    | _ -> body_instrs
  in
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
  let ret_targs = match !ret_var with
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
  [Trm (match body_ret with
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
