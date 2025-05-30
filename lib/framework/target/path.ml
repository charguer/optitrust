include Dir (* FIXME: avoid this? *)
open Ast
open Trm
open Typ
open Contextualized_error
open Mark
open Tools

(***********************************************************************************)
(*                               Auxiliary functions                               *)
(***********************************************************************************)

(** [app_to_nth_opt l n cont]: apply a continuation to the nth element of [l] if it exists, returning [None] otherwise. *)
let app_to_nth_opt (l : 'a list) (n : int) (cont : 'a -> 'b) : 'b option =
  try Option.map cont (List.nth_opt l n)
  with
  | Invalid_argument _ -> failwith "Path.app_to_nth_opt: index must be non-negative"

(** [app_to_nth cur_p l n cont]: apply a continuation to the nth element of [l] if it exists, failing at path [cur_p] otherwise. *)
let app_to_nth (cur_p : path) (l : 'a list) (n : int) (cont : 'a -> 'b) : 'b =
  match app_to_nth_opt l n cont with
  | None ->
    path_fail cur_p ("Path.app_to_nth: not enough elements (>= " ^ (string_of_int (n + 1)) ^ " expected)")
    (* LATER: report a better error message when using dArg 1 on a function with only 1 argument, for example *)
  | Some v -> v

let app_to_nth_dflt (l : 'a list) (n : int) (cont : 'a -> 'b list) : 'b list =
  Option.value ~default:[] (app_to_nth_opt l n cont)

(***********************************************************************************)
(*                                 Apply on path                                   *)
(***********************************************************************************)

let handle_path_error (p : path) (f : unit -> 'a) : 'a =
  try f ()
  with
  | Contextualized_error (contexts, e) ->
    contextualized_exn ((path_error_context p) :: contexts) e
  | e -> path_exn p e

(** [apply_on_path transfo t dl]: follow an explicit path to apply a function on the corresponding subterm *)
let apply_on_path (transfo : trm -> trm) (t : trm) (dl : path) : trm =
  let rec aux_on_path_rec (dl : path) (t : trm) : trm =
    match dl with
    | [] -> transfo t
    | d :: rest_dl ->
      let aux t = aux_on_path_rec rest_dl t in
      let aux_resource_item (h, formula) = (h, aux formula) in
      let apply_on_resource_set resource_set_dir i res =
        match resource_set_dir with
        | Resource_set_pure -> { res with pure = List.update_nth i aux_resource_item res.pure }
        | Resource_set_linear -> { res with linear = List.update_nth i aux_resource_item res.linear }
        | Resource_set_fun_contracts -> path_fail dl "apply_on_resource_set: not handled Resource_set_fun_contract"
      in
      let newt = begin match d, t.desc with
      | Dir_before _, _ ->
        (* trm_fail t *)
        path_fail dl "apply_on_path: Dir_before should not remain at this stage; probably the transformation was not expecting a target-between (tBefore, tAfter, ...)"
      | Dir_span _, _ ->
        (* trm_fail t *)
        path_fail dl "apply_on_path: Dir_span should not remain at this stage; probably the transformation was not expecting a target-span (tSpan)"
      | Dir_seq_nth n, Trm_seq (tl, result) ->
        { t with desc = Trm_seq (Mlist.update_nth n aux tl, result) }
      | Dir_cond, Trm_if (cond, then_t, else_t) ->
        { t with desc = Trm_if (aux cond, then_t, else_t)}
      | Dir_cond, Trm_while (cond, body) ->
        (* TODO: example of optimization
          let cond2 = aux cond in
          if cond2 == cond then t
          else { t with desc = Trm_while (aux cond, body)}
        *)
        { t with desc = Trm_while (aux cond, body)}
      | Dir_cond, Trm_do_while (body, cond) ->
        { t with desc = Trm_do_while (body, aux cond)}
      | Dir_cond, Trm_for_c (init, cond, step, body, contract) ->
        { t with desc = Trm_for_c (init, aux cond, step, body, contract)}
      | Dir_cond, Trm_switch (cond, cases) ->
        { t with desc = Trm_switch (aux cond, cases)}
      | Dir_then, Trm_if (cond, then_t, else_t) ->
        { t with desc = Trm_if (cond, aux then_t, else_t)}
      | Dir_else, Trm_if (cond, then_t, else_t) ->
        { t with desc = Trm_if (cond, then_t, aux else_t) }
      | Dir_var_body, Trm_let (tx,body) ->
        let body =
        begin match trm_ref_inv body with
        | Some (ty, arg) -> trm_ref ty (aux arg)
        | None -> aux body
        end in
        { t with desc = Trm_let (tx, body)}
      | Dir_let_body, Trm_let (tx, body) ->
        trm_replace (Trm_let (tx, aux body)) t
      | Dir_body, Trm_for (l_range, body, contract) ->
        { t with desc = Trm_for (l_range, aux body, contract) }
      | Dir_body, Trm_for_c (init, cond, step, body, contract) ->
        { t with desc = Trm_for_c (init, cond, step, aux body, contract) }
      | Dir_body, Trm_while (cond, body) ->
        { t with desc = Trm_while (cond, aux body)}
      | Dir_body, Trm_do_while (body, cond) ->
        trm_replace (Trm_do_while (aux body, cond)) t
      | Dir_body, Trm_abort (Ret (Some body)) ->
        { t with desc = Trm_abort (Ret (Some (aux body)))}
      | Dir_body, Trm_fun (params, tyret, body, contract) ->
        trm_replace (Trm_fun (params, tyret, aux body, contract)) t
      | Dir_for_start, Trm_for (range, body, contract) ->
        { t with desc = Trm_for ({ range with start = aux range.start }, body, contract)}
      | Dir_for_stop, Trm_for (range, body, contract) ->
        { t with desc = Trm_for ({ range with stop = aux range.stop }, body, contract)}
      | Dir_for_step, Trm_for (range, body, contract) ->
        { t with desc = Trm_for ({ range with step = aux range.step }, body, contract)}
      | Dir_for_c_init, Trm_for_c (init, cond, step, body, contract) ->
        { t with desc = Trm_for_c (aux init, cond, step, body, contract)}
      | Dir_for_c_step, Trm_for_c (init, cond, step, body, contract) ->
        { t with desc = Trm_for_c (init, cond, aux step, body, contract)}
      | Dir_app_fun, Trm_apps (f, tl, gargs, gbind) ->
        (*
          warning: the type of f may change
          -> print and reparse to have the right type
        *)
        { t with desc = Trm_apps (aux f, tl, gargs, gbind)}
      | Dir_arg_nth n, Trm_apps (f, tl, gargs, gbind) ->
        { t with desc = Trm_apps (f, List.update_nth n aux tl, gargs, gbind)}
      | Dir_ghost_arg_nth n, Trm_apps (f, tl, gargs, gbind) ->
        { t with desc = Trm_apps (f, tl, List.update_nth n aux_resource_item gargs, gbind)}
      | Dir_arg_nth n, Trm_fun (txl, tx, body, contract) ->
        let txl' =
          List.update_nth n
            (fun (x1, tx) ->
              let t' = aux (trm_var ?loc:t.loc x1) in
              match t'.desc with
              | Trm_var x' -> (x', tx)
              | _ ->
                (* trm_fail t *)
                path_fail dl ("Path.apply_on_path: transformation must preserve fun arguments")
            )
            txl
        in
        trm_replace (Trm_fun (txl', tx, body, contract)) t
      | Dir_name, Trm_let ((x,tx),body) ->
        let t' = aux (trm_var ?loc:t.loc x) in
        begin match t'.desc with
        | Trm_var x' -> { t with desc = Trm_let ((x', tx), body)}
        | _ -> (* trm_fail t *)
          path_fail dl "Path.apply_on_path: transformation must preserve variable names"
        end
      | Dir_type, Trm_let ((x, tx), body) ->
        trm_replace (Trm_let ((x, aux tx), body)) t
      | Dir_type, Trm_prim (ty, prim) ->
        trm_replace (Trm_prim (aux ty, prim)) t
      | Dir_case (n, cd), Trm_switch (cond, cases) ->
        let updated_cases =
          (List.update_nth n
            (fun (tl, body) ->
              match cd with
              | Case_body -> (tl, aux body)
              | Case_name i ->
                (List.update_nth i (fun ith_t -> aux ith_t) tl , body)
            )
            cases
          ) in
          trm_replace (Trm_switch (cond, updated_cases)) t
      | Dir_record_member n, Trm_typedef td ->
        begin match td.typedef_body with
        | Typedef_record rfl ->
          let updated_rfl =
            (List.update_nth n (fun (rf, rf_ann) ->
              match rf with
              | Record_method t1 -> (Record_method (aux t1), rf_ann )
              | _ -> path_fail dl "Path.apply_on_path: expected a method."
            ) rfl )
          in
          trm_replace (Trm_typedef {td with typedef_body = Typedef_record updated_rfl}) t
        | _ -> path_fail dl "Path.apply_on_path: transformation applied on the wrong typedef."
        end
      | Dir_namespace, Trm_namespace (name, body, inline) ->
        { t with desc = Trm_namespace (name, aux body, inline) }

      | Dir_contract (Contract_pre, resource_set_dir, i), Trm_fun (params, tyret, body, FunSpecContract contract) ->
        let pre = apply_on_resource_set resource_set_dir i contract.pre in
        trm_replace (Trm_fun (params, tyret, body, FunSpecContract { contract with pre })) t
      | Dir_contract (Contract_post, resource_set_dir, i), Trm_fun (params, tyret, body, FunSpecContract contract) ->
        let post = apply_on_resource_set resource_set_dir i contract.post in
        trm_replace (Trm_fun (params, tyret, body, FunSpecContract { contract with post })) t

      | Dir_contract (Contract_pre, resource_set_dir, i), Trm_for (range, body, contract) ->
        let pre = apply_on_resource_set resource_set_dir i contract.iter_contract.pre in
        trm_replace (Trm_for (range, body, { contract with iter_contract = { contract.iter_contract with pre } })) t
      | Dir_contract (Contract_post, resource_set_dir, i), Trm_for (range, body, contract) ->
        let post = apply_on_resource_set resource_set_dir i contract.iter_contract.post in
        trm_replace (Trm_for (range, body, { contract with iter_contract = { contract.iter_contract with post } })) t
      | Dir_contract (Contract_loop_ghosts, Resource_set_pure, i), Trm_for (range, body, contract) ->
        let loop_ghosts = List.update_nth i aux_resource_item contract.loop_ghosts in
        trm_replace (Trm_for (range, body, { contract with loop_ghosts })) t
      | Dir_contract (Contract_parallel_reads, Resource_set_linear, i), Trm_for (range, body, contract) ->
        let parallel_reads = List.update_nth i aux_resource_item contract.parallel_reads in
        trm_replace (Trm_for (range, body, { contract with parallel_reads })) t
      | Dir_contract (Contract_invariant, resource_set_dir, i), Trm_for (range, body, contract) ->
        let invariant = apply_on_resource_set resource_set_dir i contract.invariant in
        trm_replace (Trm_for (range, body, { contract with invariant })) t

      | Dir_contract (Contract_invariant, resource_set_dir, i), Trm_for_c (start, cond, incr, body, Some invariant) ->
        let invariant = apply_on_resource_set resource_set_dir i invariant in
        trm_replace (Trm_for_c (start, cond, incr, body, Some invariant)) t

      | _, _ ->
          let s = dir_to_string d in
          path_fail dl (Printf.sprintf "Path.apply_on_path: direction %s does not match with trm %s" s (Ast_to_c.ast_to_string t))

      end in
      newt
      (*{ newt with ctx = unknown_ctx () }*)
      (* TODO: restore node invalidation with option to decide if it need to be done or not *)
      (* NOTE: we don't reset the types, we need to keep them, for is_statement *)
      (* TODO: go through trm_build in order to keep track of the fact that this is a fresh AST node
        in the sense Node_to_reparse
        TODO: also search for desc = in the whole codebase
        -- TODO: make sure to use the optimization
        if t==newt then t else ... *)

  in
  handle_path_error dl (fun () -> aux_on_path_rec dl t)


(***********************************************************************************)
(*                           Explicit path resolution                              *)
(***********************************************************************************)

(** [resolve_path_and_ctx dl t]: follow the explicit path and return the corresponding subterm and its context (the visited terms with the latest first). *)
let resolve_path_and_ctx (dl : path) (t : trm) : trm * (trm list) =
  let rec aux_on_path_rec (dl : path) (t : trm) (ctx : trm list) : trm * (trm list) =
    match dl with
    | [] -> (t, ctx)
    | d :: dl_rest ->
      let aux t2 = aux_on_path_rec dl_rest t2 (t :: ctx) in
      let aux_resource_item (h, formula) = aux formula in
      let aux_resource_set resource_set_dir i res : trm * (trm list) =
        match resource_set_dir with
        | Resource_set_pure -> aux_resource_item (List.nth res.pure i)
        | Resource_set_linear -> aux_resource_item (List.nth res.linear i)
        | Resource_set_fun_contracts -> failwith "resolve_path_and_ctx: not handled Resource_set_fun_contract"
      in
      let loc = t.loc in
      begin match d, t.desc with
      | Dir_before _, _ -> trm_fail t "aux_on_path_rec: Dir_before should not remain at this stage"
      | Dir_span _, _ -> trm_fail t "aux_on_path_rec: Dir_span should not remain at this stage"
      | Dir_seq_nth n, Trm_seq (tl, result) ->
        let tl = Mlist.to_list tl in
        (* DEPRECATED:
        let decl_before (n : int) (tl : trm list) =
          List.fold_lefti
            (fun i acc (t : trm) ->
              if i >= n
                then acc
                else
                  match t.desc with
                  | Trm_let _ -> t :: acc
                  | Trm_typedef _ -> t :: acc
                  | _ -> acc) [] tl
          in
          ((decl_before n tl)@ctx)
        *)
        app_to_nth dl tl n (fun nth_t -> aux nth_t)
      | Dir_cond, Trm_if (cond, _, _)
        | Dir_cond, Trm_while (cond, _)
        | Dir_cond, Trm_do_while (_, cond)
        | Dir_cond, Trm_switch (cond, _) ->
        aux cond
      | Dir_cond, Trm_for_c (init, cond, _, _, _) ->
        aux cond
        (* DEPRECATED:
        begin match init.desc with
        | Trm_let _  -> aux cond (init :: ctx)
        | _ -> aux cond
        end *)
      | Dir_then, Trm_if (_, then_t, _) ->
        aux then_t
      | Dir_else, Trm_if (_, _, else_t) ->
        aux else_t
      | Dir_body, Trm_fun (args, _, body, _) ->
        (* DEPRECATED:
        (* do as if fun args were heap allocated *)
        let args_decl =
          List.rev_map trm_let_mut_uninit args
        in
        (args_decl@ctx) *)
        aux body
      | Dir_body, Trm_for_c (init, _, _, body, _) ->
        (* DEPRECATED:
        begin match init.desc with
        | Trm_let _ ->
            aux body (init :: ctx)
        | _ -> aux body ctx
        end *)
        aux body
      | Dir_body, Trm_for (_, body, _) ->
        aux body
      | Dir_var_body, Trm_let (_, body) ->
        (* DEPRECATED:
        let ref_op_arg = ref_operation_arg body in
        if is_ref_operation body then aux ref_op_arg (body :: ctx) else aux body ctx *)
        aux body
      | Dir_let_body, Trm_let (_, body) ->
        aux body
      | Dir_body, Trm_while (_, body)
        | Dir_body, Trm_do_while (body, _)
        | Dir_body, Trm_abort (Ret (Some body)) ->
        aux body
      | Dir_for_start, Trm_for (range, _, _) ->
        aux range.start
      | Dir_for_stop, Trm_for (range, _, _) ->
        aux range.stop
      | Dir_for_step, Trm_for (range, _, _) ->
        aux range.step
      | Dir_for_c_init, Trm_for_c (init, _, _, _, _) ->
        aux init
      | Dir_for_c_step, Trm_for_c (init, _, step, _, _) ->
        (* DEPRECATED!
        begin match init.desc with
        | Trm_let _ ->
            aux step (init :: ctx)
        | _ -> aux step ctx
        end *)
        aux step
      | Dir_app_fun, Trm_apps (f, _, _, _) -> aux f
      | Dir_arg_nth n, Trm_apps (_, tl, _, _) ->
        app_to_nth dl tl n (fun nth_t -> aux nth_t)
      | Dir_ghost_arg_nth n, Trm_apps (_, _, gl, _) ->
        app_to_nth dl gl n (fun (_, nth_t) -> aux nth_t)
      | Dir_arg_nth n, Trm_fun (arg, _, _, _) ->
        app_to_nth dl arg n
          (fun (x, typ) -> aux (trm_var ?loc ~typ x))
      | Dir_name , Trm_let ((x,typ),_) ->
        aux (trm_var ?loc ~typ x)
      | Dir_name, Trm_goto x ->
        (* CHECK: #var-id-dir-name , is this correct? *)
        aux (trm_var ?loc (name_to_var x))
      | Dir_name, Trm_typedef td ->
        aux (trm_var ?loc td.typedef_name)
      | Dir_type, Trm_let ((_, typ), _) ->
        aux typ
      | Dir_type, Trm_prim (typ, _) ->
        aux typ
      | Dir_case (n, cd), Trm_switch (_, cases) ->
        app_to_nth dl cases n
          (fun (tl, body) ->
            match cd with
            | Case_body -> aux body
            | Case_name i ->
                app_to_nth dl tl i (fun ith_t -> aux ith_t)
          )
      | Dir_enum_const (n, ecd), Trm_typedef td ->
        begin match td.typedef_body with
        | Typedef_enum xto_l ->
          app_to_nth dl xto_l n
            (fun (x, t_o) ->
              match ecd with
              | Enum_const_name -> aux (trm_var ?loc x)
              | Enum_const_val ->
                begin match t_o with
                | None ->
                    loc_fail loc
                      "Path.resolve_path_and_ctx: no value for enum constant"
                | Some t ->
                    aux t
                end
            )
        | _ -> loc_fail loc "Path.resolving_path: direction"
        end
      | Dir_record_member n, Trm_typedef td ->
        begin match td.typedef_body with
        | Typedef_record rfl ->
          app_to_nth dl rfl n
            (fun (rf, rf_annt) -> match rf with
              | Record_method t1 -> aux t1
              | _ -> trm_fail t "Path.apply_on_path: expected a method.")
        | _ -> trm_fail t "Path.apply_on_path: transformation applied on the wrong typedef."
        end
      | Dir_namespace, Trm_namespace (name, body, inline) ->
        aux body

      | Dir_contract (Contract_pre, resource_set_dir, i), Trm_fun (params, tyret, body, FunSpecContract contract) ->
        aux_resource_set resource_set_dir i contract.pre
      | Dir_contract (Contract_post, resource_set_dir, i), Trm_fun (params, tyret, body, FunSpecContract contract) ->
        aux_resource_set resource_set_dir i contract.post
      | Dir_contract (Contract_pre, resource_set_dir, i), Trm_for (range, body, contract) ->
        aux_resource_set resource_set_dir i contract.iter_contract.pre
      | Dir_contract (Contract_post, resource_set_dir, i), Trm_for (range, body, contract) ->
        aux_resource_set resource_set_dir i contract.iter_contract.post
      | Dir_contract (Contract_loop_ghosts, Resource_set_pure, i), Trm_for (range, body, contract) ->
        aux_resource_item (List.nth contract.loop_ghosts i)
      | Dir_contract (Contract_parallel_reads, Resource_set_linear, i), Trm_for (range, body, contract) ->
        aux_resource_item (List.nth contract.parallel_reads i)
      | Dir_contract (Contract_invariant, resource_set_dir, i), Trm_for (range, body, contract) ->
        aux_resource_set resource_set_dir i contract.invariant
      | Dir_contract (Contract_invariant, resource_set_dir, i), Trm_for_c (start, cond, incr, body, Some invariant) ->
        aux_resource_set resource_set_dir i invariant

      | _, _ ->
        let s = dir_to_string d in
        let s_t = Ast_to_c.ast_to_string t in
        loc_fail loc (Printf.sprintf "Path.resolve_path_and_ctx: direction  %s does not match with the following term %s" s s_t )
      end
  in
  handle_path_error dl (fun () -> aux_on_path_rec dl t [])

(** [resolve_path dl t]: resolve get the trm that corresponds to path [dl] *)
let resolve_path (dl : path) (t : trm) : trm  =
  fst (resolve_path_and_ctx dl t )


(***********************************************************************************)
(*                           Smart constructors for paths                          *)
(***********************************************************************************)

(* For debugging *)
let debug_path = false

(** [parent]: returns the parent of a path. *)
let parent (p : path) : path =
   match List.rev p with
   | _ :: p' -> List.rev p'
   | _ -> p

(** [parent_with_dir]: returns the parent of a path [p],
   checking that the direction from the parent is [d]. *)
let parent_with_dir (p : path) (d : dir) : path =
   match List.rev p with
   | d' :: p' when d == d' -> List.rev p'
   | _ -> path_fail p "Path.parent_with_dir: unexpected path"

(** [to_inner_loop] takes the path to a loop that contains 1 nested loop,
   and returns the path the inner loop *)
let to_inner_loop (p : path) : path =
  p @ [Dir_body; Dir_seq_nth 0]

(** [to_inner_loops] takes the path to a loop that contains N nested loops,
   and returns the path the inner loop *)
let rec to_inner_loop_n (n : int) (p : path) : path =
   if n > 0 then to_inner_loop_n (n - 1) (to_inner_loop p) else p


(** [index_in_surrounding_loop]: takes the path to a term inside a loop,
   and returns the index of that term in the sequence of the loop body,
   as well as the path to the loop itself. *)
let index_in_surrounding_loop (dl : path) : int * path =
   match List.rev dl with
   | Dir_seq_nth i :: Dir_body :: dl' -> (i, List.rev dl')
   | _ -> path_fail dl "Path.index_in_surrounding_loop: unexpected path"

(** [to_outer_loop]: takes the path to a loop surrounded by another loop,
   and returns the path to the outer loop *)
let to_outer_loop (p : path) : path =
  snd (index_in_surrounding_loop p)
  (* DEPRECATED:
  match index_in_surrounding_loop p with
  | (0, p') -> p'
  | _ -> path_fail p "Path.to_outer_loop: unexpected path"
  *)

(* TODO: I guess we should get Before and Span out of dir, and have a special last_dir field in every path,
   this would feel more elegant since we are forced to split this last part before resolution anyway *)
type last_dir =
  | Here
  | Nth of int
  | Before of int
  | Span of span

let extract_last_dir (p: path): path * last_dir =
  if p = [] then [], Here else
  let parent_path, dir_last = List.unlast p in
  match dir_last with
  | Dir_seq_nth i -> parent_path, Nth i
  | Dir_before i -> parent_path, Before i
  | Dir_span span -> parent_path, Span span
  | _ -> p, Here

let index_in_seq (p : path) : int * path =
   match extract_last_dir p with
   | p', Nth i -> (i, p')
   | _ -> path_fail p "Path.index_in_seq: expected a Dir_seq_nth at the end of the path; you should target instructions in a sequence"

(** [last_dir_before_inv p] for a path of the form [p1 @ Dir_before n]
   returns the pair [Some (p1,n)], else returns [None]. *)
let last_dir_before_inv (p : path) : (path * int) option =
  match extract_last_dir p with
  | parent_path, Before i -> Some (parent_path, i)
  | _ -> None

(** [extract_last_dir_before p] takes a path of the form [p1 @ Dir_before n]
   and returns the pair [(p1,n)] *)
let extract_last_dir_before (p : path) : path * int =
  if p = [] then path_fail p "Path.extract_last_dir_before does not apply to an empty path";
  match last_dir_before_inv p with
  | None ->
      if debug_path then Tools.debug "Path: %s" (path_to_string p);
      path_fail p "Path.extract_last_dir_before expects a Dir_before at the end of the path; your target is probably missing a target_relative modifier, e.g. tBefore or tFirst."
  | Some res -> res

(** [extract_last_dir_span] takes a path that ends inside a sequence and return the corresponding span. *)
let extract_last_dir_span (p: path) : path * span =
  match extract_last_dir p with
  | parent_path, Span span -> parent_path, span
  | parent_path, Nth i -> parent_path, Dir.span_around i
  | parent_path, Before i -> parent_path, Dir.span_before i
  | _ ->
      if debug_path then Tools.debug "Path: %s" (path_to_string p);
      path_fail p "Path.extract_last_dir_span expects the last direction to be inside a sequence."


(** [split_common_prefix]: given paths [a] and [b], returns [(p, ra, rb)]
   such that [a = p @ ra] and [b = p @ rb] *)
let split_common_prefix (a : path) (b : path) : path * path * path =
   let rec aux (rev_p : path) (ra : path) (rb : path) =
      match (ra, rb) with
      | (dir_a :: ra, dir_b :: rb) when dir_a = dir_b ->
         aux (dir_a :: rev_p) ra rb
      | _ -> ((List.rev rev_p), ra, rb)
   in
   aux [] a b

(** [identify_common_seq_span]: given a previously identified [seq] and a [span],
  updates the sequence and span to be a common prefix with a span including [p_instr]. *)
let identify_common_seq_span (seq : path) (span : Dir.span) (p_instr : path) : path * Dir.span =
  let (prefix, p1, p2) = split_common_prefix seq p_instr in
  if p1 = []
  then begin
    let (i2, _) = index_in_seq p2 in
    (prefix, Dir.span_including span i2)
  end else begin
    let (i1, _) = index_in_seq p1 in
    let (i2, _) = index_in_seq p2 in
    let span = { start =  min i1 i2; stop = max i1 i2 } in
    (prefix, span)
  end

(* TODO: move elsewhere to Paths ? *)
let add_marks_at_paths ?(prefix : path = []) (ps:path list) (t:trm) : trm * mark list =
  let marks = List.map (fun _ -> Mark.next()) ps in
  (* LATER: could use a system to set all the marks in a single pass over the ast,
      able to hand the Dir_before *)
  let res = List.fold_left2 (fun t p m ->
    let p = prefix @ p in
    match extract_last_dir p with
    | p_to_seq, Before i -> apply_on_path (trm_add_mark_between i m) t p_to_seq
    | p_to_seq, Span span -> apply_on_path (trm_add_mark_span span m) t p_to_seq
    | _ -> apply_on_path (trm_add_mark m) t p)
    t ps marks
    in
  res, marks

(* given a path in an arithmetic expression and the entire ast,
   find the root of that expression,
   otherwise immediately return the provided path.
   returns (path_to_trm, path). *)
let find_arith_expr_root (p : path) (t : trm) : (trm * path) =
  (* TODO: it is worth factorizing with 'find_surrounding_instr'?
    'find_surrounding_trm_such_that' ? *)
  (* TODO: is it worth having some kind of check?
  assert ((Option.is_some (trm_lit_inv t)) || (Option.is_some (trm_var_inv t)) || (is_prim_arith_call t)); *)
  let p_t, ctx = resolve_path_and_ctx p t in
  let rec aux p t ctx =
    match ctx with
    | parent_t :: ctx ->
      let parent_p = parent p in
      if not (is_prim_arith_call parent_t) then (t, p)
      else aux parent_p parent_t ctx
    | [] -> (t, p)
  in
  aux p p_t ctx

(* given a path in a term, go up this path until hitting an instruction.
   returns that instruction and its path. *)
let find_surrounding_instr (p : path) (t : trm) : path =
  let p_t, ctx = resolve_path_and_ctx p t in
  let rec aux p p_t ctx =
    if trm_is_statement p_t then p else begin match ctx with
    | parent_t :: ctx -> aux (parent p) parent_t ctx
    | [] -> failwith "could not find surrounding instruction"
    end
  in
  aux p p_t ctx

(** Given a path to a sub-expression of an instruction,
    returns the path to the instruction, and the rest of the path.
    Given a path to an instruction, returns it. *)
let path_in_instr (p : path) (t : trm) : (path * path) =
  let to_instr = find_surrounding_instr p t in
  let to_expr = List.drop (List.length to_instr) p in
  (to_instr, to_expr)
