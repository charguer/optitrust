
open Prelude
open Target
open Path
open Mlist
include Apac_basic
include Apac_core

(* [constify tg]: expects target [tg] to point at a function definition. It
   constifies function arguments and functions whenever it is possible depending
   on data accesses, aliases and dependencies. *)
let constify (tg : target) : unit =
  (* Step 1: Clear the hash table of constification records. *)
  Var_Hashtbl.clear Apac_core.const_records;
  (* Step 2: Iterate over [tg] and fill the hash table of constification
     records. *)
  Apac_core.build_constification_records tg;
  (* Step 3: Clear the stack of arguments and functions to unconstify after the
     analysis of data accesses, aliases and dependencies. Indeed, the
     constification algorithm begins by consedring that all functions and
     function arguments can be constified. It then performs an analysis (in
     Step 4) of data accesses, aliases and dependencies and unconstifies all of
     the variables (aliases), arguments and functions where the constification
     is not possible. *)
  Stack.clear Apac_core.to_unconst;
  (* Step 4: Perform an analysis of data accesses, aliases and dependencies,
     then decide which variables (aliases), arguments and functions should not
     be constified. *)
  Apac_core.identify_mutables tg;
  (* Step 5: Propagate the unconstification. *)
  Apac_core.unconstify_mutables ();
  (* Step 6: Effectively transform the AST so as to add 'const' keywords to
     function arguments, functions and *)
  Apac_basic.constify_args tg;
  (* Step 7: aliases of function arguments. Here, we begin by clearing the
     global hash table of multiple variable declarations that are subject to
     partial constification, i.e. one or more of the underlying variable
     declarations should be constified. *)
  Hashtbl.clear Apac_core.to_const_mult;
  (* We then perform the constification of aliases. However, this transformation
     cannot perform partial constification. In such cases, it only collects the
     necessary information about the concerned multiple variable declarations
     and inserts it into [Apac_core.to_const_mult]. *)
  Apac_basic.constify_aliases tg;
  (* At the end, we iterate over the key-value pairs of
     [Apac_core.to_const_mult], unfold the concerned multiple variable
     declarations having a specific mark (see [Apac_core.const_mult] and the
     comments in [Apac_basic.constify_arguments]) into sequences of simple
     variable declarations and constify those declaration which should be
     constified. *)
  Hashtbl.iter (fun k v ->
      Apac_basic.unfold_let_mult ~constify:v (tg @ [cMark k])
    ) Apac_core.to_const_mult

(* [unfold_function_calls tg]: expects target [tg] to point at a function
   definition. It moves all function calls under target [tg] out of variable
   declarations and nested function calls.

    Example:

          int a = f(g(2));

    becomes:

          int __var_1;
          __var_1 = g(2);
          int __var_2;
          __var_2 = f(__var_1);
          int a = __var_2;

    However:

          int a;
          a = f(g(2));

    becomes:

          int a;
          int __var_1;
          __var_1 = g(2);
          a = f(__var_1);

    as the call to 'f' is already dissociated from the declaration of 'a'. See
    also comments within the function.
*)
let unfold_function_calls (tg : target) : unit =
  Target.iter (fun t p ->
    (* Get the parent term to check whether it is an assignment (outside of a
       declaration). If it is the case, we do not need to apply the
       transformation. It would only create a superfluous variable. *)
    let parent_path = Path.parent p in
    let parent_target = target_of_path parent_path in
    if not (is_set_operation (get_trm_at_exn parent_target))
    then begin
      (* Define new intermediate variable. *)
      let var = fresh_var_name ~prefix:"__var_" () in
      (* Bind the return value of the current function call to that variable. *)
      Variable_basic.bind var (target_of_path p);
      (* Separate the assignment of the return value from the declaration of the
         variable. *)
      Variable_basic.init_detach [cVarDef var];
    end
  ) tg

(* [parallel_task_group tg]: expects target [tg] to point at a function
    definition.

    The first step of the transformation consists in replacing return statements
    by gotos. At the beginning of the process, the function's body is wrapped
    into a sequence to which a mark is assigned.
    See [Apac_basic.use_goto_for_return] for more details.

    In the second step, we put the marked sequence into an OpenMP task group.
    See [Apac_basic.task_group] for more details. *)
let parallel_task_group : Transfo.t =
  Target.iter (fun t p ->
    (* Create a mark. *)
    let mark = Mark.next() in
    (* Wrap the target function's body into a marked sequence and replace return
       statements by gotos. *)
    Apac_basic.use_goto_for_return ~mark (target_of_path p);
    (* Get the name of the target function through the deconstruction of the
       corresponding AST term. *)
    let error =
    "Apac.parallel_task_group: expected a target to a function definition" in
    let (qvar, _, _, _) = trm_inv ~error trm_let_fun_inv (
      Path.get_trm_at_path p t
    ) in
    (* Transform the marked instruction sequence corresponding to the target
       function's body into an OpenMP task group.

       Note that if the target function is the 'main' function, we want the
       task group to be executed only by one thread, the master thread. *)
    Apac_basic.task_group ~master:(var_has_name qvar "main") [cMark mark];
    (* 5) Remove the mark. *)
    Marks.remove mark [cMark mark];
  )
(*
(* [decl_cptrs]: hashtable storing available varaibles and whether they are
    arrays. *)
type decl_cptrs = (var, bool) Hashtbl.t

(* [get_delete_task ptrs]: returns an OpenMP task that deletes the variable
    contained in [ptrs]. Each variable has an InOut dependency. *)
let get_delete_task (ptrs : decl_cptrs) : trm option =
  let vars = Tools.hashtbl_keys_to_list ptrs in
  match vars with
  | [] -> None
  | _ ->
    let deps : deps = List.map (fun var -> Dep_ptr (Dep_var var) ) vars in
    let delete_trms : trms = List.map (fun var -> trm_delete (Hashtbl.find ptrs var) (trm_var var)) vars in
    Some (trm_add_pragmas [Task [Depend [Inout deps]]] (trm_seq_nomarks delete_trms))

(* [heapify_nested_seq tg]: expects target [tg] to point at a sequence. Then, it
    will:
    - send on the heap all variables declared in this scope,
    - dereference all use of them (add  '*'),
    - add task to delete these variables before 'return', 'break' and 'continue'
      statements when needed. *)
let heapify_nested_seq : Transfo.t =
  Target.iter (fun t p ->
    (* Heapifies variables and adds 'delete' tasks before certain Trm_abort. *)
    let rec aux (ptrs : decl_cptrs) (is_first_depth : bool) (t : trm) : trm =
      match t.desc with
      (* New scope *)
      | Trm_seq _ -> trm_map (aux (Hashtbl.copy ptrs) false) t
      | Trm_for _ | Trm_for_c _  -> trm_map (aux (Hashtbl.copy ptrs) false) t
      | Trm_while _ | Trm_switch _ | Trm_if _ -> trm_map (aux ptrs false) t

      | Trm_let (_, (var, ty), _, _) ->
        (* Remove variable from occurs when declaring them again. *)
        if Hashtbl.mem ptrs var then begin Hashtbl.remove ptrs var; trm_map (aux ptrs is_first_depth) t end
        (* Heapify new variable only for first depth. *)
        else if is_first_depth then begin
          let tr = Apac_basic.stack_to_heap_aux t in
          Hashtbl.add ptrs var (is_typ_array (get_inner_ptr_type ty));
          trm_map (aux ptrs is_first_depth) tr
          end
        else trm_map (aux ptrs is_first_depth) t
      | Trm_let_mult (_, tvl, tl) ->
        (* Raises error on partial heapify. *)
        let has_defined_var = List.fold_left (fun has_defined_var (var, ty) ->
          if Hashtbl.mem ptrs var then begin Hashtbl.remove ptrs var; true end
          else if not has_defined_var then begin Hashtbl.add ptrs var (is_typ_array ty); false end
          else fail None "Apac.heapify_nested_seq.aux: partial heapify of Trm_let_mult"
        ) false tvl in
        if has_defined_var
          then trm_map (aux ptrs is_first_depth) t
          else trm_map (aux ptrs is_first_depth) (Apac_basic.stack_to_heap_aux t)
      (* Dereference heapified variables. *)
      | Trm_var (kind, qv) when Hashtbl.mem ptrs qv && not (Hashtbl.find ptrs qv) -> trm_get t
      (* Add delete task before:
          - 'return': everytime,
          - 'break', 'continue': only the current loop, not deeper. *)
      | Trm_abort _ when is_return t || is_first_depth ->
        begin match get_delete_task ptrs with
        | Some (tr) -> trm_seq_no_brace [tr; trm_map (aux ptrs is_first_depth) t]
        | _ -> t
        end
      | _ -> trm_map (aux ptrs is_first_depth) t
    in
    (* Add a 'delete' task at the end of the sequence if there is not
       Trm_abort at the end. *)
    let add_end_delete_task (ptrs : decl_cptrs) (t :trm) : trm =
      match t.desc with
      | Trm_seq tl ->
        begin match List.rev (Mlist.to_list tl) with
        | tr :: _ when is_trm_abort tr -> t
        | _ ->
          begin match get_delete_task ptrs with
          | Some (tr) -> trm_seq_add_last (tr) t
          | _ -> t
          end
        end
      | _ -> t
    in
    let decl_cptrs = Hashtbl.create 10 in
    let tg_trm = Path.get_trm_at_path p t in
    match tg_trm.desc with
    | Trm_seq tl -> Nobrace_transfo.remove_after (fun _ ->
      transfo_on_targets (trm_map (aux decl_cptrs true)) (target_of_path p));
      transfo_on_targets (add_end_delete_task decl_cptrs) (target_of_path p)
    | _ -> fail None "Apac.heapify_nested_seq: Expects target to point at a sequence"
  )

(* [get_all_vars acc t]: returns the list of variables used in
    application [t]. *)
let rec get_all_vars (acc: vars) (t : trm) : vars =
  match t.desc with
  | Trm_apps (_, tl) -> List.fold_left get_all_vars acc tl
  | Trm_var (_, qv) when not (List.mem qv acc) -> qv :: acc
  | _ -> acc

(* [get_dep var ty]: returns the dep of the typ [ty]. *)
let get_dep (var : var) (ty : typ) : dep =
  let rec aux (depth : int) : dep =
    if depth > 0 then Dep_ptr (aux (depth-1)) else Dep_var var
  in
  aux (typ_get_degree ty)

(* [sync_with_taskwait tg]: expects target [tg] to point at a function
    definition. Then, it will add 'taskwait' OpenMP pragmas before loop and
    conditional branches. It also adds it at the end of loops. *)
let sync_with_taskwait : Transfo.t =
  Target.iter (fun t p ->
    let add_taskwait (decl_vars : (var, dep) Hashtbl.t) (vars : vars) (t : trm) : trm =
      match vars with
      | [] -> t
      | _ ->
        let deps = List.map (fun var -> Hashtbl.find decl_vars var) vars in
        trm_add_pragma (Taskwait [Depend [Inout deps]]) t
    in
    let add_taskwait_end_seq (decl_vars : (var, dep) Hashtbl.t) (vars : vars) (t : trm) : trm =
      match t.desc with
      | Trm_seq _ ->
        begin match vars with
        | [] -> t
        | _ ->
          let deps = List.map (fun var -> Hashtbl.find decl_vars var) vars in
          (* Trick : use empty Trm_arbitrary to add taskwait alone (trm_unit
             adds semi colons. *)
          let taskwait = trm_add_pragma (Taskwait [Depend [Inout deps]]) (code (Expr "")) in
          trm_seq_add_last taskwait t
        end
      | _ -> fail None "Apac.sync_with_taskwait.add_taskwait_end_seq: expected sequence"
    in
    (* Adds taskwait at the end of loops. *)
    (* TODO: loop: taskwait at the end, check if variable in condition is used
             in function.
      Currently, it always adds taskwait. *)
    let add_taskwait_loop_body (decl_vars : (var, dep) Hashtbl.t) (vars : vars) (t : trm) : trm =
      match t.desc with
      | Trm_while (cond , body) ->
        let new_body = add_taskwait_end_seq decl_vars vars body in
        trm_alter ~desc:(Trm_while (cond, new_body)) t
      | Trm_do_while (body, cond) ->
        let new_body = add_taskwait_end_seq decl_vars vars body in
        trm_alter ~desc:(Trm_do_while (new_body, cond)) t
      | Trm_for (l_range, body, contract) ->
        let new_body = add_taskwait_end_seq decl_vars vars body in
        trm_alter ~desc:(Trm_for (l_range, new_body, contract)) t
      | Trm_for_c (init, cond, step, body, contract) ->
        let new_body = add_taskwait_end_seq decl_vars vars body in
        trm_alter ~desc:(Trm_for_c (init, cond, step, new_body, contract)) t
      | _ -> t
    in
    (* Removes the [n] last elements of list [l]. *)
    let remove_n (n : int) (l : 'a list) : 'a list =
      let rec aux (n : int) (l : 'a list) : 'a list =
        match l with
        | [] -> []
        | _ :: t -> if n <= 0 then l else aux (n-1) t
      in
      List.rev (aux n (List.rev l))
    in
    let rec aux (decl_vars : (var, dep) Hashtbl.t) (t : trm) : trm =
      match t.desc with
      | Trm_seq _ -> trm_map (aux (Hashtbl.copy decl_vars)) t
      | Trm_let (_, (var, ty), _, _) -> Hashtbl.add decl_vars var (get_dep var (get_inner_ptr_type ty)); t
      | Trm_let_mult (_, tvl, _) ->
        List.iter (fun (var, ty) -> Hashtbl.add decl_vars var (get_dep var (get_inner_ptr_type ty))) tvl; t
      | Trm_if (cond, _, _) | Trm_switch (cond , _) ->
        trm_map (aux (Hashtbl.copy decl_vars)) (add_taskwait decl_vars (get_all_vars [] cond) t)
      | Trm_while (cond, _) ->
        let l = get_all_vars [] cond in
        let t = add_taskwait_loop_body decl_vars l t in
        trm_map (aux (Hashtbl.copy decl_vars)) (add_taskwait decl_vars l t)
      | Trm_do_while (_, cond) ->
        let l = get_all_vars [] cond in
        let t = add_taskwait_loop_body decl_vars l t in
        trm_map (aux (Hashtbl.copy decl_vars)) t
      | Trm_for ((var, _, _, _, step, _), _, _) ->
        let l = begin match step with
        | Step tr -> get_all_vars [var] tr
        | _ -> [var]
        end in
        let t = add_taskwait_loop_body decl_vars l t in
        trm_map (aux (Hashtbl.copy decl_vars)) (add_taskwait decl_vars (remove_n 1 l) t)
      | Trm_for_c (init, cond, step, _, _) ->
        let (l, n) = begin match init.desc with
        | Trm_let (_, (var, ty), _, _) ->
          Hashtbl.add decl_vars var (get_dep var ty);
          (get_all_vars [var] cond, 1)
        | Trm_let_mult (_, tvl, _) ->
          let l = List.fold_left (fun acc (var, _) -> var :: acc) [] tvl in
          List.iter (fun (var, ty) -> Hashtbl.add decl_vars var (get_dep var ty)) tvl;
          (get_all_vars l cond, List.length l)
        | _ -> (get_all_vars [] cond, 0)
        end in
        let l = get_all_vars l step in
        let t = add_taskwait_loop_body decl_vars l t in
        trm_map (aux (Hashtbl.copy decl_vars)) (add_taskwait decl_vars (remove_n n l) t)
      | Trm_abort Ret (Some tr) -> add_taskwait decl_vars (get_all_vars [] tr) t
      | _ -> t
    in
    let tg_trm = Path.get_trm_at_path p t in
    let decl_vars = Hashtbl.create 10 in
    match tg_trm.desc with
    | Trm_let_fun (qv, ty, args, body, _) ->
      List.iter (fun (var, ty) -> if var.name <> "" then Hashtbl.add decl_vars var (get_dep var ty)) args;
      transfo_on_targets (trm_map (aux decl_vars)) (target_of_path p)
    | _ -> fail None "Apac.sync_with_taskwait: Expects target to point at a function declaration"
  )

(* [fun_loc]: function's Unified Symbol Resolution *)
type fun_loc = var

(* [dep_info]: stores data of dependencies of tasks. *)
type dep_info = {
  dep_depth : int;
  dep_in : bool;
  dep_shared : bool;
}

(* [fun_args_deps]: hashtable storing dep_info for each function argument. *)
type fun_args_deps = (fun_loc, dep_info list) Hashtbl.t

(* [dep_infos]: list of dep_info and the corresponding variables. *)
type dep_infos = (var * dep_info) list

(* [vars_depth]: hashtable storing the pointer depth of variable and its name.
    The name of the variable is in the key and the value in order to use
    Apac_basic.get_vars_data_from_cptr_arith. *)
type vars_depth = var vars_tbl

(* [is_base_type ty]: checks whether [ty] is a base type or not. *)
let rec is_base_type (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_int | Typ_float | Typ_double | Typ_bool | Typ_char | Typ_string | Typ_unit -> true
  | Typ_constr _ ->
    begin match Apac_core.typ_get_alias ty with
    (* alias *)
    | Some (ty) -> is_base_type ty
    (* 'class', 'struct', 'union', 'enum' ... *)
    | None -> true
    end
  | _ -> false

(* [is_dep_in ty]: checks whether the OpenMP dependency type of [ty] is In. It
    does not handle auto! *)
let rec is_dep_in (ty : typ) : bool =
  (* Returns true if there is 'const' before every pointer and before the base
     type. *)
  let rec aux (ty : typ) : bool =
    match ty.typ_desc with
    (* Unwrap alias. *)
    | Typ_constr _ | Typ_const _ when Apac_core.typ_is_alias (get_inner_const_type ty) ->
      begin match Apac_core.typ_get_alias (get_inner_const_type ty) with
      | Some (ty) -> aux ty
      | None -> assert false
      end
    (* Base type 'const' *)
    | Typ_const ty when is_base_type ty -> true
    (* Pointer const *)
    | Typ_const { typ_desc = Typ_ptr { ptr_kind = Ptr_kind_mut ; inner_typ = ty } } -> aux ty
    | Typ_const { typ_desc = Typ_array (ty, _); _ } -> aux ty
    (* Pointer *)
    | Typ_ptr { ptr_kind = Ptr_kind_mut; _ } -> false
    | Typ_array (ty, _) -> false
    (* Base type *)
    | _  when is_base_type ty -> false
    (* Should not be encountered. *)
    | Typ_ptr { ptr_kind = Ptr_kind_ref; _ } -> assert false
    | Typ_const _ -> assert false
    | _ -> assert false
  in
  match ty.typ_desc with
  (* Unwrap alias *)
  | Typ_constr _ | Typ_const _ when Apac_core.typ_is_alias (get_inner_const_type ty) ->
    begin match Apac_core.typ_get_alias (get_inner_const_type ty) with
    | Some (ty) -> is_dep_in ty
    | None -> assert false
    end
  (* Reference *)
  | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
    begin match ty.typ_desc with
    (* 'void &' *)
    | Typ_unit -> fail None "is_dep_in: void & as argument"
    (* 'const void &' *)
    | Typ_const { typ_desc = Typ_unit } -> fail None "is_dep_in: const void & as argument"
    | _ -> aux ty
    end
  (* 'const void' *)
  | Typ_const { typ_desc = Typ_unit } -> fail None "is_dep_in: const void as argument"
  (* Base type *)
  | _ when is_base_type ty -> true
  | _ -> aux ty

(* [get_functions_args_deps tg]: expects target [tg] to point at the root. Then,
    it will return the dep_info of each function argument. *)
let get_functions_args_deps (tg : target) : fun_args_deps =
  let rec aux (fad : fun_args_deps) (is_method : bool) (t : trm) : unit =
    match t.desc with
    | Trm_seq _ | Trm_namespace _ -> trm_iter (aux fad is_method) t
    | Trm_typedef { typdef_body = Typdef_record _; _ } -> trm_iter (aux fad true) t
    | Trm_let_fun (qv, _, tvl, _, _) when qv.name <> "main" ->
      let fc = Ast_data.get_function_usr_unsome t in
      let args_info = List.map (fun (var, ty) ->
        let dep_in = is_dep_in ty in
        let is_record = match Context.record_typ_to_typid ty with | Some (_) -> true | None -> false in
        {dep_depth = Apac_core.typ_get_degree (get_inner_ptr_type ty);
        dep_in = dep_in;
        dep_shared = is_reference ty || (is_record && dep_in);}) tvl
      in
      let args_info = if is_method
        then begin
          let is_const_method = trm_has_cstyle Const_method t in
          {dep_depth = -1;
          dep_in = is_const_method;
          dep_shared = is_const_method;} :: args_info
          end
        else args_info
      in
      Hashtbl.add fad fc args_info
    | _ -> ()
  in
  let tg_trm = match get_trm_at tg with
    | Some (t) when trm_is_mainfile t -> t
    | _ -> fail None "Apac.constify_functions_arguments: expected a target to the main file sequence"
  in
  let fad = Hashtbl.create 10 in
  aux fad false tg_trm;
  fad

(* [count_unop_get t]: returns the number of nested unary operations of [t]. *)
let count_unop_get (t : trm) : int =
  let rec aux (count : int) (t : trm) : int =
    match t.desc with
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [tr]) -> aux (count+1) tr
    | Trm_apps (_, tl) -> List.fold_left aux count tl
    | _ -> count
  in
  aux 0 t

(* [is_unary_mutation t]: checks if [t] is a primitive unary operaiton that
    mutates a variable. *)
let is_unary_mutation (t : trm) : bool =
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop uo)); _}, _) ->
    begin match uo with
    | Unop_post_dec | Unop_post_inc | Unop_pre_dec | Unop_pre_inc -> true
    | _ -> false
    end
  | _ -> false

(* [get_unary_mutation_qvar t]: returns fully qualified name of a variable
    behind unary mutatation operator. *)
let get_unary_mutation_qvar (t : trm) : var =
  let rec aux (t : trm) : var =
    match t.desc with
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop (Unop_get | Unop_address))); _}, [t]) -> aux t
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop (Binop_array_access))); _}, [t; _]) -> aux t
    | Trm_var (_, name)-> name
    | _ -> new_var "hello"
  in
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop uo)); _}, [tr]) ->
    begin match uo with
    | Unop_post_dec | Unop_post_inc | Unop_pre_dec | Unop_pre_inc -> aux tr
    | _ -> new_var "hello"
    end
  | _ -> new_var "hello"

(* [get_apps_deps vd fad t]: gets the list of dependencies of trm [t]. Expects
    the transformation [unfold_funcall] to be applied before. The returned list
    may have duplicates or different dependencies for the same variable, so it
    must be post-processed. *)
let get_apps_deps (vd : vars_depth) (fad : fun_args_deps) (t : trm) : dep_infos =
  let rec aux (dis : dep_infos) (t : trm) : dep_infos =
    match t.desc with
    (* Function call *)
    | Trm_apps ({ desc = Trm_var _} as f, args) ->
      (*let (_, qvar) = trm_inv trm_var_inv f in
      let _ = Printf.printf "Getting deps of function: %s\n" qvar in*)
      let l = Hashtbl.find fad (Ast_data.get_function_usr_unsome f) in
      List.fold_left2 (fun acc ({dep_depth; dep_in; _} as dep_info) t ->
        match (Apac_core.trm_strip_accesses_and_references_and_get t).desc with
        | Trm_var (_, qv) ->
          let di = { dep_info with dep_depth = if dep_depth = -1 then (count_unop_get t) - 1 else dep_depth}  in
          (qv.name, di) :: acc
        | Trm_apps (_, tl) ->
          (* Resolve pointers. *)
          let dep_infos = if dep_depth > 1  && not dep_in
            then begin match Apac_basic.get_vars_data_from_cptr_arith vd t with
            | Some (var) -> (var, dep_info) :: dis
            | None -> dis
            end
            else dis in
          (* Recursive call to add the other variables. *)
          List.fold_left aux dep_infos tl
        | _  -> dis
        ) dis l args
    (* Set operation *)
    | Trm_apps (_, [lhs; rhs]) when is_set_operation t ->
      begin match (Apac_core.trm_strip_accesses_and_references_and_get lhs).desc with
      | Trm_var (_, qv) ->
        let (depth, _) = Hashtbl.find vd qv.qvar_str in
        let di = { dep_depth = depth; dep_in = false; dep_shared = true;} in
        aux ((qv.qvar_str, di) :: dis) rhs
      | _ -> assert false
      end
    (* Unary mutation *)
    | Trm_apps (_, [tr]) when is_unary_mutation t ->
      let qv = get_unary_mutation_qvar t in
      let (depth, _) = Hashtbl.find vd qv.qvar_str in
      let di = { dep_depth = depth; dep_in = false; dep_shared = true;} in
      (qv.qvar_str, di) :: dis
    (* Explore further. *)
    | Trm_apps (_, tl) -> List.fold_left aux dis tl
    (* Variable *)
    | Trm_var (_, qv) ->
      let _ = Printf.printf "Trying to find %s\n" qv.qvar_str in
      let (depth, _) = Hashtbl.find vd qv.qvar_str in
      let _ = Printf.printf "Found %s\n" qv.qvar_str in
      let di = { dep_depth = depth; dep_in = true; dep_shared = false;} in
      (qv.qvar_str, di) :: dis
    | _ -> dis
  in
  aux [] t

(* [sort_dep_infos dis]: returns the the list of dependencies for In and InOut
    dependencies from dep_info list [dis]. *)
let sort_dep_infos (dis : dep_infos) : (dep_infos * dep_infos) =
  let dep_in_tbl : (var, dep_info) Hashtbl.t = Hashtbl.create 10 in
  let dep_inout_tbl : (var, dep_info) Hashtbl.t = Hashtbl.create 10 in
  List.iter (fun (var, di) ->
    if di.dep_in && not (Hashtbl.mem dep_inout_tbl var) then Hashtbl.add dep_in_tbl var di
    else if not di.dep_in then
      begin match Hashtbl.find_opt dep_inout_tbl var with
      | Some (arg_info) when arg_info.dep_shared -> ()
      | _ -> Hashtbl.remove dep_in_tbl var; Hashtbl.replace dep_inout_tbl var di
      end
    ) dis;
  (Tools.hashtbl_to_list dep_in_tbl, Tools.hashtbl_to_list dep_inout_tbl)

(* [get_cpagma_from_dep_infos dis_in dis_inout]: returns the cpragma
    corresponding to a task.

    [dis_in] - list of In dependencies,
    [dis_inout] - list of InOut dependencies. *)
let get_cpagma_from_dep_infos (dis_in : dep_infos) (dis_inout : dep_infos) : cpragma =
  let rec aux (var : var) (depth : int) : dep =
    if depth > 0 then Dep_ptr (aux  var (depth-1)) else Dep_var var
  in
  let is_empty l : bool =
    match l with | [] -> true | _ -> false
  in
  let deps_in : deps = List.map (fun (var, {dep_depth; _}) -> aux var dep_depth) dis_in in
  let deps_inout : deps = List.map (fun (var, {dep_depth; _}) -> aux var dep_depth) dis_inout in
  let shared_vars = List.fold_left (fun acc (var, {dep_shared; _}) ->
    if dep_shared then var :: acc else acc) [] dis_inout in
    let depend = if is_empty deps_in then [] else [In deps_in] in
    let depend = if is_empty deps_inout then depend else Inout deps_inout :: depend in
    let clauses = if is_empty depend then [] else [Depend depend] in
    let clauses = if is_empty shared_vars then clauses else Shared shared_vars :: clauses in
    Task clauses

(* [insert_tasks_naive fad]: expects target [tg] to point at a function
    definition. Then, it will insert task in the function's body.

    This is a naive implementation that adds a task on every
    instruction/application. *)
let insert_tasks_naive (fad : fun_args_deps) : Transfo.t =
  Target.iter (fun t p ->
    let rec aux (vd : vars_depth) (t : trm) : trm =
      match t.desc with
      (* New sequence *)
      | Trm_seq _ | Trm_if _ | Trm_for _ | Trm_for_c _ | Trm_while _ | Trm_do_while _ | Trm_switch _ ->
        trm_map (aux (Hashtbl.copy vd))  t
      (* New variable *)
      | Trm_let (_, (var, ty), { desc = Trm_apps (_, [tr]); _ }, _) ->
        Hashtbl.add vd var (Apac_core.typ_get_degree (get_inner_ptr_type ty), var); t
      | Trm_let_mult (_, tvl, _) ->
        List.iter (fun (var, ty) -> Hashtbl.add vd var (Apac_core.typ_get_degree ty, var)) tvl; t
      (* New task *)
      | Trm_apps _ ->
        let dis = get_apps_deps vd fad t in
        let (dis_in, dis_inout) = sort_dep_infos dis in
        let t = trm_seq_nomarks [t] in
        trm_add_pragma (get_cpagma_from_dep_infos dis_in dis_inout) t
      | _ -> t
    in
    let tg_trm = Path.get_trm_at_path p t in
    let vd = Hashtbl.create 10 in
    match tg_trm.desc with
    | Trm_let_fun (_, _, tvl, _, _) ->
      List.iter (fun (var, ty) -> if var <> "" then Hashtbl.add vd var (Apac_core.typ_get_degree ty, var)) tvl;
      Target.apply_at_target_paths (aux vd) (target_of_path (p @ [Dir_body]))
    | _ -> fail None "Apac.insert_tasks_naive: expected a target to a function definition"
  )
 *)
