open Ast
open Target

(* [fold ~as_reference ~fold_at tg] *)
let fold ?(as_reference : bool = false) ?(fold_at : target list = [[]]) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.fold as_reference fold_at i t p) tg


(* [insert ~const ~as_reference x dx tg] *)
let insert ?(const : bool = false) ?(as_reference : bool = false) (x : var) (dx : var) (tg : target) : unit =
  Trace.apply (fun ctx t ->
   let ps = resolve_target_between tg t in
   List.fold_left (fun t (p,i) -> Variable_core.insert ctx const as_reference x dx i t p) t ps
  )

(* [insert ~const ~as_reference x dx tg] *)
(* let insert ?(const : bool = false) ?(as_reference : bool = false) (x : var) (dx : var) (tg : target) : unit =
  Target.apply_on_target_between
    (fun t (p,i) -> Variable_core.insert const as_reference x dx i t p) tg *)

(* [remove tg] *)
let remove : Transfo.t =
  Generic.remove_instruction

(* [insert_and_fold ~const ~as_reference ~fold_at x dx tg] *)
let insert_and_fold ?(const : bool = false) ?(as_reference : bool = false) ?(fold_at : target list = [[]]) (x : var) (dx : string) (tg : target) : unit =
  Trace.apply (fun ctx t ->
   let ps = resolve_target_between tg t in
   List.fold_left (fun t (p, i) -> Variable_core.insert_and_fold ctx const as_reference x dx i fold_at t p) t ps
  )
  

(* [insert_and_fold ~const ~as_reference ~fold_at x dx tg] *)
(* let insert_and_fold ?(const : bool = false) ?(as_reference : bool = false) ?(fold_at : target list = [[]]) (x : var) (dx : trm) (tg : target) : unit =
  (* TODO: apply_on_target_between *)
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.insert_and_fold const as_reference x dx i fold_at t p) tg *)

(* [inline ~delete_decl ~inline_at tg] *)
let inline ?(delete_decl : bool = false) ?(inline_at : target list = [[]]) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.inline delete_decl inline_at i t p) tg

(*
  instr containing f(arg1, …, argn) is replaced with
  {
    x1 = arg1
    …
    xn = argn
    decl result
    body[x1, …, xn][return r := {result = r; goto return_label}]
    return_label:
      instr[f(arg1, …, argn) := result]
  }
  if tf is void, result won't be used, but instead the empty statement
 *)
 (*
let inline_fun_decl ?(inline_at : target list = [[]]) (result : var)  ?(fun_args : var list = [])
  (return_label : label) (f : var) (tf : typ) (args : typed_var list)
  (body : trm) (t : trm) : trm =

  (*let counter = ref 0 in  ^ string_of_int !counter *)
  (* inline f everywhere in t *)
  let rec apply_change (t : trm) : trm =
    (*Ast_to_text.print_ast ~only_desc:true stdout t; *) (* TODO: make show_ast  accessible from everywhere*)
    (* new names replacing the argument names *)
    (* DEPRECATED incr counter; *)
  let fresh_args =
     match fun_args with
     | [] -> List.map (fun (x, tx) -> (Generic_core.fresh_in t x, tx)) args
     | _ ->
      if List.length fun_args <> List.length args
        then fail t.loc "inline_fun_decl: incorrect number of names provided for the arguments";
       List.map2 (fun (_x, tx) nx -> (nx, tx)) args fun_args
     in

    (* name for the result of f, result might be an argument name *)
  let result =
      Generic_core.fresh_in
        (trm_seq
          (t ::
              List.map
                (fun x_tx -> trm_let Var_mutable x_tx (trm_lit Lit_uninitialized))
                fresh_args
          )
        )
        result
    in
    (* result is heap allocated *)
    let result_decl =
      trm_let Var_mutable (result, typ_ptr tf) (trm_prim (Prim_new tf))

    in
    (* body where the argument names are substituted *)
    let body =
      List.fold_left2
        (fun body (x, _) nx ->
          Generic_core.change_trm
            (trm_var x)
            (trm_apps ~annot:(Some Mutable_var_get) (trm_unop Unop_get)
              [trm_var nx])
            body
        )
        body
        args fun_args
    in
    (* body where res is used instead of return statements *)
    let replace_return (t : trm) : trm =
      let rec aux (t : trm) : trm =
        match t.desc with
        (* remove delete instruction related to return statement if any *)
        (* | Trm_seq tl when t.annot = Some Delete_instructions ->
          begin match List.rev tl with
          | {desc = Trm_abort (Ret (Some r)); _} :: _ ->
              trm_seq ~annot:(Some No_braces) ~loc:t.loc
                [trm_set ~loc:t.loc ~is_statement:true (trm_var result) r;
                trm_goto ~loc:t.loc return_label]
          | {desc = Trm_abort (Ret None); _} :: _ ->
              trm_goto ~loc:t.loc return_label
          | _ -> trm_map aux t
          end *)
        | Trm_abort (Ret (Some r)) ->
          trm_seq ~annot:(Some No_braces) ~loc:t.loc
            [trm_set ~loc:t.loc ~is_statement:true (trm_var result) r;
              trm_goto ~loc:t.loc return_label]
        | Trm_abort (Ret None) -> trm_goto ~loc:t.loc return_label
        | _ -> trm_map aux t
      in
      Generic_core.clean_up_no_brace_seq (aux t)
    in
    let body = replace_return body in
    let bodyl =
      match body.annot with
      (* | Some Delete_instructions -> [body] *)
      | _ ->
        begin match body.desc with
        | Trm_seq tl -> tl
        | _ -> [body]
        end
    in

    (* we look for instructions that contain a call to f *)
    if not (t.is_statement && contains_call_to_fun f t)
    then trm_map apply_change t
    else
      let arg_vals = fun_call_args f t in
      if List.length fresh_args <> List.length arg_vals
        then fail t.loc "inline_fun_decl: incorrect number";

      let arg_decls =
        List.map2
          (fun (x, tx) dx ->
             trm_let ~is_statement:true Var_mutable (x,tx) dx
          )
          fresh_args
          arg_vals
      in

      let t =
        match tf.typ_desc with
        | Typ_unit ->
            trm_seq ~loc:t.loc ~annot:(Some No_braces)
                (arg_decls ++ bodyl ++
                 [
                   trm_labelled return_label
                     (Generic_core.change_trm (trm_apps (trm_var f) arg_vals)
                        (trm_lit Lit_unit) t)
                 ]
                )
                
           begin match arg_dels with
           (* if no args, no delete instruction *)
           | [] ->
              trm_seq ~loc:t.loc
                (bodyl ++
                 [
                   trm_labelled return_label
                     (Generic_core.change_trm (trm_apps (trm_var f) arg_vals)
                        (trm_lit Lit_unit) t)
                 ]
                )
           | _ ->
              trm_seq ~annot:(Some Delete_instructions)
                ((trm_seq ~loc:t.loc
                    (bodyl ++
                     [let inline_record_access (clog : out_channel) (field : string) (var : string ) (t : trm) : trm =
      (* Ast_to_text.print_ast ~only_desc:true stdout t; *)
      let pl = [cVarDef var] in
      let epl = resolve_target pl t in
      let var_decl = match epl with
      | [dl] -> let (t_def,_) = resolve_path dl t in t_def

      | _ ->  fail t.loc "inline_record_access: expected a type"
      in
      let var_type ,only_decl = match var_decl.desc with
      | Trm_seq tl->
        let only_decl = if List.length tl = 1 then true else false
        in
        let t_decl = List.hd tl in
        begin match t_decl.desc with
        | Trm_let (_,(x, var_typ), _) when x = var ->
          begin match var_typ.typ_desc with
          | Typ_ptr {typ_desc=Typ_constr (y, _, _);_} -> y ,only_decl
          | _ -> fail t.loc "inline_record_access: type was not matched"
          end
        | _ -> fail t.loc "inline_record_access: expected a declaration"
        end
      | _ -> fail t.loc "inline_record_access: could not match the sequnce which contains the declaration"
      in

      let list_of_trms = if not only_decl then match var_decl.desc with

        | Trm_seq [_;t_assign] ->
          begin match t_assign.desc with
          | Trm_apps (_,[_;lt]) ->
            begin match lt.desc with
            | Trm_struct tl -> tl
            | _ -> fail t.loc "implicit_record_assignment: expected a record"
            end
          |  _ -> fail t.loc "implicit_record_assignment: expected an assignment"
          end
        |  _ -> fail t.loc "implicit_record_assignment: expected a sequence term"

        else (* search for the trms,
                assumption the variable is only once assigned*)
          (* let loc_pl = [cSet ~lhs:[cVar var ]()] in  *)
          let loc_pl = cSet ~lhs:[cVar var] ()  in
          let loc_epl = resolve_target loc_pl t in
          match loc_epl with
          | [dl] -> let (t_assgn,_) = resolve_path dl t in
            begin match t_assgn.desc with
            | Trm_apps(_,[_;rs]) ->
              begin match rs.desc with
              | Trm_struct tl -> tl
              | _ -> fail t.loc "inline_struct_access: expected a record"
              end
            | _ -> fail t.loc "inline_struct_access: expected an assignment"
            end
          | _ -> fail t.loc "inline_struct_access: assumed that the variable was assigned only once"
      in
      let struct_decl_path = [cTypDef var_type ] in
      let epl_of_struct_decl = resolve_target struct_decl_path t in
      let struct_decl_trm  = match epl_of_struct_decl with
        | [dl] -> let (t_def,_) = resolve_path dl t in t_def
        | _ -> fail t.loc "inline_struct_access: expected a typedef struct"
      in
      let app_transfo (t : trm) (dl : path) : trm =
        match List.rev dl with
        | Dir_nth _ :: dl' ->
          let dl = List.rev dl' in
          apply_on_path (inline_record_access_core clog var field struct_decl_trm list_of_trms) t dl
        | _ -> fail t.loc "inline_struct_access:expected a dir_nth inside the sequence"
      in
      match epl with
      | [] ->
        print_info t.loc "inline_struct_access: no matching subterm";
        t
      | _ -> List.fold_left (fun t dl -> app_transfo t dl) t epl
      (* Ast_to_text.print_ast ~only_desc:true stdout var_decl; *)

*)