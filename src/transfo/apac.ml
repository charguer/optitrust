
open Ast
open Target
open Path
open Mlist
include Apac_basic

let insert_timer_start (tg : target) : unit =
  Target.apply(fun t p ->
    let p_seq,i = Path.last_dir_before_inv_success p in 
    Path.apply_on_path(fun t_seq ->
      let instrs = trm_inv ~error:"expected a sequence" trm_seq_inv t_seq in
      let app_to_insert = trm_apps (trm_var "timer_start") [] in
      let timer = Mlist.insert_at i app_to_insert instrs in
      trm_seq ~annot:t_seq.annot timer
    ) t p_seq
  ) tg

(* [parallel_task_group ~mark tg]: expects target [ŧg] to point at a taskable
    function definition. Then, it will insert:

      #pragma omp parallel
      #pragma omp master
      #pragma omp taskgroup

    in the body of the main function or

      #pragma omp taskgroup

    in the body of another function. *)
let parallel_task_group ?(mark : mark = "") : Transfo.t =
  Target.iter ( fun t p ->
    Apac_basic.use_goto_for_return ~mark (target_of_path p);
    Target.apply_at_target_paths ( fun t ->
      match t.desc with
      | Trm_let_fun (qvar, ret_typ, args, body, contract) ->
        let body_tl = match trm_seq_inv body with
        | Some (tl) -> Mlist.map (fun t ->
          match t.desc with
          | Trm_seq _ ->
            let pragmas = if qvar.qvar_str = "main" then [Parallel []; Master ; Taskgroup] else [Taskgroup] in
            trm_add_pragmas pragmas t
          | _ -> t
          ) tl
        | None -> assert false
        in
        trm_alter ~desc:(Trm_let_fun(qvar, ret_typ, args, (trm_seq body_tl), contract)) t
      | _ -> assert false
      ) (target_of_path p)
    )

(* [fun_loc]: function's Unified Symbol Resolution *)
type fun_loc = string

(* [arg_const]: argument constification record, it tells whether an argument and
    its dependencies downstream should be constified. *)
type arg_const = {
  (* is the argument a refrence or a pointer? *)
  is_ptr_or_ref : bool;
  (* should the argument be constified? *)
  mutable is_arg_const : bool;
  (* list of other arguments that depend on this argument *)
  mutable dependency_of : (fun_loc * int) list;
  (* typedef id of the type if it is a record, -1 otherwise *)
  tid : typconstrid;
}

(* [fun_const]: function constification record, it tells whether a function and
    its arguments should be constified. *)
type fun_const = {
  (* information for each argument. For methods, the object is added as the
     first argument, except for constructors and destructors. *)
  args_const : arg_const list;
  (* depth of pointer of the return type *)
  ret_ptr_depth : int;
  (* is the return type a reference? *)
  is_ret_ref : bool;
  (* is it a method? set to false for constructors and destructors *)
  is_method : bool;
}

(* [fun_args_const]: hashtable storing [fun_const] of functions.
    The key of the hashtable is the function's Unified Symbol Resolution. *)
type fun_args_const = (fun_loc, fun_const) Hashtbl.t

(* [constifiable]: hashtable storing constification records for all of the
    functions. *)
type constifiable = (fun_loc, (bool list * bool)) Hashtbl.t

(* [is_cptr_or_ref ty]: checks if [ty] is a reference or a pointer type. *)
let is_cptr_or_ref (ty : typ) : bool =
  match (get_inner_const_type ty).typ_desc with
  | Typ_ptr _ | Typ_array _-> true
  | _ -> false

(* [get_binop_set_left_var t]: returns the variable name on the left side of a
    set operator and a boolean indicating if the variable has been
    dereferenced. *)
let get_binop_set_left_var (t : trm) : (var * bool) option =
  let rec aux (is_deref : bool) (t : trm) : (var * bool) option =
    match t.desc with
    | Trm_var (_, qv) -> Some(qv.qvar_str, is_deref)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop (Binop_array_access))); _ }, [t; _]) -> aux true t
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop (uo))); _ }, [t]) ->
      begin match uo with
      | Unop_get  | Unop_struct_access _ -> aux true t
      | Unop_struct_get label -> Some (label, is_deref)
      | _ -> aux is_deref t
      end
    | _ -> None
  in
  aux false t

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
let get_unary_mutation_qvar (t : trm) : qvar =
  let rec aux (t : trm) : qvar =
    match t.desc with
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop (Unop_get | Unop_address))); _}, [t]) -> aux t
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop (Binop_array_access))); _}, [t; _]) -> aux t
    | Trm_var (_, name)-> name
    | _ -> empty_qvar
  in
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop uo)); _}, [tr]) ->
    begin match uo with
    | Unop_post_dec | Unop_post_inc | Unop_pre_dec | Unop_pre_inc -> aux tr
    | _ -> empty_qvar
    end
  | _ -> empty_qvar


(* [identify_constifiable_functions tg]: expects target [tg] to point at the
    root. Then, it will return the corresponding constifiable function. *)
let identify_constifiable_functions (tg : target) : constifiable =
  (* TODO : better handle include files *)
  let tg_trm = match get_trm_at tg with
    | Some (t) when trm_is_mainfile t -> t
    | _ -> fail None "Apac.constify_functions_arguments: expected a target to the main file sequence"
  in
  let fac : fun_args_const = Hashtbl.create 10 in
  (* Store the function's arguments (fname, nth arg) to unconst. *)
  let to_process : (fun_loc * int) Stack.t = Stack.create () in
  (* Get the list of function declarations trms. *)
  let get_fun_decls (t : trm) : (trm * int) list =
    let rec aux (t : trm) : (trm * int) list =
      match t.desc with
      | Trm_seq tl -> Mlist.fold_left (fun acc t ->
        match t.desc with
        | Trm_let_fun _ -> (t, -1) :: acc
        | Trm_namespace (name, body, is_inline) -> acc @ (aux body)
        | Trm_typedef ({ typdef_body = Typdef_record _; _ } as td) ->
          acc @ (List.fold_left (fun acc tr -> (tr, td.typdef_typid) :: acc) [] (typedef_get_methods t))
        | _ when List.exists (function | Include _ -> true | _ -> false) (trm_get_files_annot t) ->
          acc @ (aux t)
        | _ -> acc
        ) [] tl
      | _ -> assert false
    in
    aux t
  in
  let (fun_decls, tids) = List.split (get_fun_decls tg_trm) in
  (* Helper function : add element to process in 'to_process'. *)
  let add_elt_in_to_process (va : vars_arg) (usr : fun_loc) (name : var) : unit =
    (* We also take the previously pointed arguments because aliasing creates
       dependencies.
      ex : a is const and b is not :
      void (int * a, int * b) { | void (int const * const a, int b) {
        int * p = a;            | int const * p = a;
        p = c;                  | p = c;
        *p = 1;                 | *p = 1;  // error : a is const --> p is const
      }                         | }
    *)
    let l = Hashtbl.find_all va name in
    List.iter (fun (_, arg_idx) -> Stack.push (usr, arg_idx) to_process) l
  in
  (* Helper function: check if the term [t] is in fac. *)
  let fac_mem_from_trm (t : trm) : bool =
    begin match Ast_data.get_function_usr t with
    | Some (usr) -> Hashtbl.mem fac usr
    | None -> false
    end
  in
  (* Helper function: get the value of the term [t] in fac. *)
  let fac_find_from_trm (t : trm) : fun_const =
    Hashtbl.find fac (Ast_data.get_function_usr_unsome t)
  in
  (* Initialize fac. *)
  List.iter2 (fun t tid ->
    match t.desc with
    | Trm_let_fun (qv, ty, args, _, _) ->
      let is_method = tid <> -1 in
      let acs = List.map (fun (_, ty) -> {
          is_ptr_or_ref = is_cptr_or_ref ty ;
          is_arg_const = true;
          dependency_of = [];
          tid = match Context.record_typ_to_typid ty with | Some(tid) -> tid | None -> -1;}
        ) args in
      (* Add the object as the first argument for object. *)
      let acs = if is_method
        then { is_ptr_or_ref = true; is_arg_const = true; dependency_of = []; tid = tid } :: acs
        else acs in
      (* Methods declared outside the class are not detected as methods
         previously. *)
      let usr = Ast_data.get_function_usr_unsome t in
      if is_method then Hashtbl.remove fac usr;
      Hashtbl.add fac usr {
        args_const = acs;
        ret_ptr_depth = Apac_basic.get_cptr_depth ty;
        is_ret_ref = is_reference ty;
        is_method = is_method }
    | _ -> assert false
    ) fun_decls tids;
  (* Update fac dependency_of and fill to_process. *)
  let update_fac_and_to_process (va : vars_arg) (cur_usr : string) (is_method : bool) (t : trm) : unit =
    let rec aux (va : vars_arg) (t : trm) : unit =
      match t.desc with
      (* new scope *)
      | Trm_seq _ | Trm_for _ | Trm_for_c _ ->
        trm_iter (aux (Hashtbl.copy va)) t
      (* The syntax allows to declare variable in the condition statement but
         clangml currently cannot parse it. *)
      | Trm_if _ | Trm_switch _ | Trm_while _ ->
        trm_iter (aux (Hashtbl.copy va)) t
      (* funcall : update dependency_of. *)
      | Trm_apps ({ desc = Trm_var (_ , funcall_name); _ } as f, args) when fac_mem_from_trm f ->
        let {args_const; _} = fac_find_from_trm f in
        List.iteri (fun i t ->
          match (Apac_basic.get_inner_all_unop_and_access t).desc with
          | Trm_var (vk, arg_name) when Hashtbl.mem va arg_name.qvar_str ->
            let (_, arg_idx) = Hashtbl.find va arg_name.qvar_str in
            let ac = List.nth args_const i in
            if ac.is_ptr_or_ref then ac.dependency_of <- (cur_usr, arg_idx) :: ac.dependency_of
          | _ -> ()
          ) args;
        trm_iter (aux va) t
      (* Declare new ref/ptr that refer/point to argument : update vars_arg. *)
      | Trm_let (_, _, { desc = Trm_apps (_, [tr]); _ }) ->
        Apac_basic.update_vars_arg_on_trm_let
          (fun () -> ())
          (fun () -> ())
          (fun () -> ())
          va t;
        trm_iter (aux va) t
      | Trm_let_mult (_, tvl, tl) ->
        List.iter2 (fun (lname, ty) t ->
          Apac_basic.update_vars_arg_on_trm_let_mult_iter
            (fun () -> ())
            (fun () -> ())
            (fun () -> ())
            va lname ty t
        ) tvl tl;
        trm_iter (aux va) t
      (* Assignment & compound assignment to argument : update to_process. *)
      | Trm_apps _ when is_set_operation t ->
        begin match set_inv t with
        | Some (lhs, rhs) ->
          begin match get_binop_set_left_var lhs with
          | Some (var_name, is_deref) when Hashtbl.mem va var_name ->
            (* Variable [var_name] has been modified, add it in to_process. *)
            add_elt_in_to_process va cur_usr var_name;
            (* Change the pointed data for not dereferenced pointers. *)
            begin match Apac_basic.get_vars_data_from_cptr_arith va rhs with
            | Some (arg_idx) when not is_deref ->
              let (depth, _) = Hashtbl.find va var_name in
              Hashtbl.add va var_name (depth, arg_idx)
            | _ -> ()
            end
          | _ -> () (* Example variable not in va : global variable *)
          end
        | None -> assert false
        end;
        trm_iter (aux va) t
      (* Mutable unary operator (++, --) : update to_process. *)
      | Trm_apps _ when is_unary_mutation t ->
        let name = get_unary_mutation_qvar t in
        if Hashtbl.mem va name.qvar_str then add_elt_in_to_process va cur_usr name.qvar_str;
        trm_iter (aux va) t
      (* Return argument : update to_process if return ref or ptr. *)
      | Trm_abort (Ret (Some tr)) ->
        let {ret_ptr_depth; is_ret_ref; _} = Hashtbl.find fac cur_usr in
        if is_ret_ref then
          begin match trm_var_inv tr with
          | Some var_name when Hashtbl.mem va var_name -> add_elt_in_to_process va cur_usr var_name
          | _ -> ()
          end
        else if ret_ptr_depth > 0 then
          begin match Apac_basic.get_vars_data_from_cptr_arith va tr with
          | Some (arg_idx) -> Stack.push (cur_usr, arg_idx) to_process
          | None -> ()
          end;
        trm_iter (aux va) t

      | _ -> trm_iter (aux va) t
    in
    aux va t
  in
  List.iter (fun t ->
    let va = Hashtbl.create 10 in
    match t.desc with
    | Trm_let_fun (qv, _, args, body) ->
      (* Add class attributes. *)
      let {is_method; args_const} = fac_find_from_trm t in
      if is_method then begin
        let tid = (List.hd args_const).tid in
        begin match Context.typid_to_trm tid with
        | Some (t) ->
          List.iter (fun (name, ty) ->
            Hashtbl.add va name (Apac_basic.get_cptr_depth ty, 0)
          ) (typedef_get_members t)
        | None -> assert false
        end
      end;
      (* Add arguments. *)
      List.iteri (fun i (name, ty) ->
        if name <> "" then
          let i = if is_method then i+1 else i in
          Hashtbl.add va name (Apac_basic.get_cptr_depth ty, i)
        ) args;
      update_fac_and_to_process va (Ast_data.get_function_usr_unsome t) is_method body
    | _ -> assert false
    ) fun_decls;
  (* Propagate argument unconstification through function call. *)
  let rec unconstify_propagate (to_process : (fun_loc * int) Stack.t) : unit =
    match Stack.pop_opt to_process with
    | Some (fun_loc, nth) ->
      let {args_const; _} = Hashtbl.find fac fun_loc in
      let ac = List.nth args_const nth in
      if ac.is_arg_const then begin
        ac.is_arg_const <- false;
        List.iter (fun e -> Stack.push e to_process) ac.dependency_of
      end;
      unconstify_propagate to_process
    | None -> ()
  in
  unconstify_propagate to_process;
  (* Create constifiable from fac. *)
  let cstfbl = Hashtbl.create 10 in
  Hashtbl.iter (fun fun_loc {args_const; is_method; _} ->
    let is_const = List.map (fun {is_arg_const; _} -> is_arg_const) args_const in
    let is_method_const = if is_method then List.hd is_const else false in
    let is_args_const = if is_method then List.tl is_const else is_const in
    Hashtbl.add cstfbl fun_loc (is_args_const, is_method_const)
    ) fac;
  cstfbl

(* [constify_functions_arguments cstfbl tg]: expects target [tg] to point at a
    function definition, then it will add "const" keyword to its arguments
    whenever it is possible. *)
let constify_functions_arguments (cstfbl : constifiable) : Transfo.t =
  Target.iter ( fun tt p ->
    let tr = Path.get_trm_at_path p tt in
    let tg = nbAny :: target_of_path p in
    match tr.desc with
    | Trm_let_fun _ ->
      begin match (Hashtbl.find_opt cstfbl (Ast_data.get_function_usr_unsome tr)) with
      | Some (is_args_const, is_method_const) ->
        Apac_basic.constify_args ~is_args_const ~is_method_const tg;
        Apac_basic.constify_args_alias ~is_args_const tg
      | None -> ()
      end
    | _ -> fail None "Apac.constify_functions_arguments: expected target to function definition."
  )

(* [decl_cptrs]: Hashtable that stores the available varaibles and if they are arrays. *)
type decl_cptrs = (var, bool) Hashtbl.t

(* [get_delete_task ptrs]: returns a omp task that deletes the variable contained in [ptrs].
    Each variable has a Inout dependence *)
let get_delete_task (ptrs : decl_cptrs) : trm option =
  let vars = Tools.hashtbl_keys_to_list ptrs in
  match vars with
  | [] -> None
  | _ ->
    let deps : deps = List.map (fun var -> Dep_ptr (Dep_var var) ) vars in
    let delete_trms : trms = List.map (fun var -> trm_delete (Hashtbl.find ptrs var) (trm_var var)) vars in
    Some (trm_add_pragmas [Task [Depend [Inout deps]]] (trm_seq_nomarks delete_trms))

(* [heapify_nested_seq tg]: expect target [tg] to point at a sequence, then it will :
    - send on the heap all variables declared in this scope,
    - dereference all use of them (add  "*")
    - add task to delete these variables before "return" "break" and "continue" statements when it is needed
  *)
let heapify_nested_seq : Transfo.t =
  iter_on_targets (fun t p ->

    (* heapifies variables and adds delete tasks before certain Trm_abort*)
    let rec aux (ptrs : decl_cptrs) (is_first_depth : bool) (t : trm) : trm =
      match t.desc with
      (* new scope *)
      | Trm_seq _ -> trm_map (aux (Hashtbl.copy ptrs) false) t
      | Trm_for _ | Trm_for_c _  -> trm_map (aux (Hashtbl.copy ptrs) false) t
      | Trm_while _ | Trm_switch _ | Trm_if _ -> trm_map (aux ptrs false) t

      | Trm_let (_, (var, ty), _, _) ->
        (* remove variable from occurs when declaring them again *)
        if Hashtbl.mem ptrs var then begin Hashtbl.remove ptrs var; trm_map (aux ptrs is_first_depth) t end
        (* heapify new variable only for first depth *)
        else if is_first_depth then begin
          let tr = Apac_basic.stack_to_heap_aux t in
          Hashtbl.add ptrs var (is_typ_array (get_inner_ptr_type ty));
          trm_map (aux ptrs is_first_depth) tr
          end
        else trm_map (aux ptrs is_first_depth) t
      | Trm_let_mult (_, tvl, tl) ->
        (* raises error if partial heapify *)
        let has_defined_var = List.fold_left (fun has_defined_var (var, ty) ->
          if Hashtbl.mem ptrs var then begin Hashtbl.remove ptrs var; true end
          else if not has_defined_var then begin Hashtbl.add ptrs var (is_typ_array ty); false end
          else fail None "Apac.heapify_nested_seq.aux: partial heapify of Trm_let_mult"
        ) false tvl in
        if has_defined_var
          then trm_map (aux ptrs is_first_depth) t
          else trm_map (aux ptrs is_first_depth) (Apac_basic.stack_to_heap_aux t)

      (* dereference heapified variables *)
      | Trm_var (kind, qv) when Hashtbl.mem ptrs qv.qvar_str && not (Hashtbl.find ptrs qv.qvar_str) -> trm_get t

      (* add delete task before :
          - return : everytime
          - break, continue : only the current loop not deeper *)
      | Trm_abort _ when is_return t || is_first_depth ->
        begin match get_delete_task ptrs with
        | Some (tr) -> trm_seq_no_brace [tr; trm_map (aux ptrs is_first_depth) t]
        | _ -> t
        end

      | _ -> trm_map (aux ptrs is_first_depth) t
    in

    (* add a delete task a the end of the sequence if there is not Trm_abort at the end *)
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
    | Trm_seq tl -> Internal.nobrace_remove_after (fun _ ->
      transfo_on_targets (trm_map (aux decl_cptrs true)) (target_of_path p));
      transfo_on_targets (add_end_delete_task decl_cptrs) (target_of_path p)
    | _ -> fail None "Apac.heapify_nested_seq: Expects target to point at a sequence"
  )


(* [get_all_vars acc t]: returns the list of variables used in the application [t]. *)
let rec get_all_vars (acc: vars) (t : trm) : vars =
  match t.desc with
  | Trm_apps (_, tl) -> List.fold_left get_all_vars acc tl
  | Trm_var (_, qv) when not (List.mem qv.qvar_str acc) -> qv.qvar_str :: acc
  | _ -> acc

(* [get_dep var ty]: returns the dep of the typ [ty] *)
let get_dep (var : var) (ty : typ) : dep =
  let rec aux (depth : int) : dep =
    if depth > 0 then Dep_ptr (aux (depth-1)) else Dep_var var
  in
  aux (get_cptr_depth ty)

(* [sync_with_taskwait tg]: expects the target [tg] to point at a function definition,
    then it will add taskwait omp pragmas before loop and conditional branches,
    it also adds it at the end of loops. *)
let sync_with_taskwait : Transfo.t =
  iter_on_targets (fun t p ->

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
          (* trick : use empty Trm_arbitrary to add taskwait alone (trm_unit adds semi colons*)
          let taskwait = trm_add_pragma (Taskwait [Depend [Inout deps]]) (code (Expr "")) in
          trm_seq_add_last taskwait t
        end
      | _ -> fail None "Apac.sync_with_taskwait.add_taskwait_end_seq: expected sequence"
    in

    (* adds taskwait at the end of loops. *)
    (* TODO : loop : taskwait at the end, check if variable in cond used in function.
      currently always add taskwait *)
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

    (* removes the [n] last element of the list [l]. *)
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
      List.iter (fun (var, ty) -> if var <> "" then Hashtbl.add decl_vars var (get_dep var ty)) args;
      transfo_on_targets (trm_map (aux decl_vars)) (target_of_path p)
    | _ -> fail None "Apac.sync_with_taskwait: Expects target to point at a function declaration"
  )


(* [gen_id ()]: unique integer generator. *)
let gen_id = Tools.fresh_generator ()

(* [count_calls t]: count the number of function call in [t]. *)
let count_calls (t : trm) : int =
  let rec aux (count : int) (t : trm) : int =
    match t.desc with
    | Trm_apps (f, args) ->
      let count = match f.desc with | Trm_var _ -> count+1 | _ -> count in
      List.fold_left (fun acc t -> aux acc t) count args
    | _ -> count
  in
  aux 0 t

(* [unfold_funcalls tg]: expects the user to have filled [fun_defs] in Ast_data (Ast_data.fill_fun_defs_tbl).
    It will search all function calls under from the target [tg]. Then it will create a new variable, change its value
    with the function call and replace the function targeted with then newly created variable. *)
let unfold_funcalls : Transfo.t =

  let unfold_funcalls_transfo (tg_instr : target) (tg_call : target) : unit =
    let tr_call = get_trm_at_exn tg_call in
    let ty = match tr_call.desc with
      | Trm_apps ({desc = Trm_var _} as f, args) ->
        let def = Ast_data.get_function_def f in
        let (_, ty, _, _) = trm_inv trm_let_fun_inv def in
        ty
      | _ -> assert false
    in
    let new_name = "__var_" ^ (string_of_int (gen_id())) in
    let tr_let_var = trm_let_mut (new_name, get_inner_const_type ty) (trm_uninitialized ()) in
    let tr_set = trm_set (trm_var new_name) tr_call in

    if not (same_types ty (typ_unit ()))
    then begin
      transfo_on_targets (fun _ -> trm_apps (trm_unop Unop_get) [trm_var new_name]) tg_call;
      Internal.nobrace_remove_after (fun _ ->
        transfo_on_targets (fun t -> trm_seq_no_brace [tr_let_var; tr_set; t]) tg_instr);
      end
    else ()
  in

  iter_on_targets (fun t p ->
    let fixed_tg = (target_of_path p ) @ [nbAny; cFun ""] in

    iteri_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
      (fun i t (path_to_seq, local_path, i1)  ->
        let path_to_instruction = path_to_seq @ [Dir_seq_nth i1] in
        let path_to_call = path_to_instruction @ local_path in
        let tg_instr = target_of_path path_to_instruction in
        let tg_call = target_of_path path_to_call in
        let tg_out_trm = Path.resolve_path path_to_instruction t in

        match tg_out_trm.desc with
        | Trm_let _ -> unfold_funcalls_transfo tg_instr tg_call
        | Trm_apps (_, [lhs; rhs]) when is_set_operation tg_out_trm && count_calls rhs >= 2 ->
          unfold_funcalls_transfo tg_instr tg_call
        | Trm_apps ({ desc = Trm_var _ }, _) when count_calls tg_out_trm >= 2->
          unfold_funcalls_transfo tg_instr tg_call
        | _ -> ()
        ) fixed_tg;
    )


(* [dep_info]: stores data of dependencies of tasks. *)
type dep_info = {
  dep_depth : int;
  dep_in : bool;
  dep_shared : bool;
}

(* [fun_args_deps]: hashtable that store the dep_info for each argument of functions. *)
type fun_args_deps = (fun_loc, dep_info list) Hashtbl.t

(* [dep_infos]: list of dep_info and its corresponding variable. *)
type dep_infos = (var * dep_info) list
(* [vars_depth]: hashtable that stores the pointer depth of variable and its name.
    The name of the variable is in the key and the value in order to use Apac_basic.get_vars_data_from_cptr_arith. *)
type vars_depth = var vars_tbl

(* [is_base_type ty]: checks if [ty] is a base type or not. *)
let rec is_base_type (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_int | Typ_float | Typ_double | Typ_bool | Typ_char | Typ_string | Typ_unit -> true
  | Typ_constr _ ->
    begin match get_inner_typedef_alias ty with
    (* alias *)
    | Some (ty) -> is_base_type ty
    (* class, struct, union, enum ... *)
    | None -> true
    end
  | _ -> false

(* [is_dep_in ty]: checks if the omp dependency type of [ty] in In.
    It does not handle auto ! *)
let rec is_dep_in (ty : typ) : bool =
  (* returns true if there is "const" before every pointer and before the base type *)
  let rec aux (ty : typ) : bool =
    match ty.typ_desc with
    (* unwrap alias *)
    | Typ_constr _ | Typ_const _ when is_typdef_alias (get_inner_const_type ty) ->
      begin match get_inner_typedef_alias (get_inner_const_type ty) with
      | Some (ty) -> aux ty
      | None -> assert false
      end
    (* base type const *)
    | Typ_const ty when is_base_type ty -> true
    (* ptr const *)
    | Typ_const { typ_desc = Typ_ptr { ptr_kind = Ptr_kind_mut ; inner_typ = ty } } -> aux ty
    | Typ_const { typ_desc = Typ_array (ty, _); _ } -> aux ty
    (* ptr *)
    | Typ_ptr { ptr_kind = Ptr_kind_mut; _ } -> false
    | Typ_array (ty, _) -> false
    (* base type *)
    | _  when is_base_type ty -> false
    (* should not encounter *)
    | Typ_ptr { ptr_kind = Ptr_kind_ref; _ } -> assert false
    | Typ_const _ -> assert false
    | _ -> assert false
  in

  match ty.typ_desc with
  (* unwrap alias *)
  | Typ_constr _ | Typ_const _ when is_typdef_alias (get_inner_const_type ty) ->
    begin match get_inner_typedef_alias (get_inner_const_type ty) with
    | Some (ty) -> is_dep_in ty
    | None -> assert false
    end
  (* reference *)
  | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
    begin match ty.typ_desc with
    (* void & *)
    | Typ_unit -> fail None "is_dep_in: void & as argument"
    (* const void & *)
    | Typ_const { typ_desc = Typ_unit } -> fail None "is_dep_in: const void & as argument"

    | _ -> aux ty
    end
  (* const void *)
  | Typ_const { typ_desc = Typ_unit } -> fail None "is_dep_in: const void as argument"
  (* base type *)
  | _ when is_base_type ty -> true

  | _ -> aux ty

(* [get_functions_args_deps tg]: expect target [tg] to point at the root,
    then it will return the dep_info of each argument of functions. *)
let get_functions_args_deps (tg : target) : fun_args_deps =
  let rec aux (fad : fun_args_deps) (is_method : bool) (t : trm) : unit =
    match t.desc with
    | Trm_seq _ | Trm_namespace _ -> trm_iter (aux fad is_method) t
    | Trm_typedef { typdef_body = Typdef_record _; _ } -> trm_iter (aux fad true) t
    | Trm_let_fun (qv, _, tvl, _, _) when qv.qvar_str <> "main" ->
      let fc = Ast_data.get_function_usr_unsome t in
      let args_info = List.map (fun (var, ty) ->
        let dep_in = is_dep_in ty in
        let is_record = match Context.record_typ_to_typid ty with | Some (_) -> true | None -> false in
        {dep_depth = Apac_basic.get_cptr_depth (get_inner_ptr_type ty);
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

(* [count_unop_get t]: return the number of nested unary operation of [t] *)
let count_unop_get (t : trm) : int =
  let rec aux (count : int) (t : trm) : int =
    match t.desc with
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [tr]) -> aux (count+1) tr
    | Trm_apps (_, tl) -> List.fold_left aux count tl
    | _ -> count
  in
  aux 0 t

(* [get_apps_deps vd fad t]: gets the list of dependency the trm [t] has.
    Expects the transformation [unfold_funcall] applied before.
    The returned list may have duplicates or different dependencies for the same variable,
    so it must be processed afterward. *)
let get_apps_deps (vd : vars_depth) (fad : fun_args_deps) (t : trm) : dep_infos =

  let rec aux (dis : dep_infos) (t : trm) : dep_infos =
    match t.desc with
    (* function call *)
    | Trm_apps ({ desc = Trm_var _} as f, args) ->
      let l = Hashtbl.find fad (Ast_data.get_function_usr_unsome f) in
      List.fold_left2 (fun acc ({dep_depth; dep_in; _} as dep_info) t ->
        match (Apac_basic.get_inner_all_unop_and_access t).desc with
        | Trm_var (_, qv) ->
          let di = { dep_info with dep_depth = if dep_depth = -1 then (count_unop_get t) - 1 else dep_depth}  in
          (qv.qvar_str, di) :: acc

        | Trm_apps (_, tl) ->
          (* resolve pointers *)
          let dep_infos = if dep_depth > 1  && not dep_in
            then begin match Apac_basic.get_vars_data_from_cptr_arith vd t with
            | Some (var) -> (var, dep_info) :: dis
            | None -> dis
            end
            else dis in
          (* recursive call to add the other variables *)
          List.fold_left aux dep_infos tl
        | _  -> dis
        ) dis l args

    (* set operation *)
    | Trm_apps (_, [lhs; rhs]) when is_set_operation t ->
      begin match (Apac_basic.get_inner_all_unop_and_access lhs).desc with
      | Trm_var (_, qv) ->
        let (depth, _) = Hashtbl.find vd qv.qvar_str in
        let di = { dep_depth = depth; dep_in = false; dep_shared = true;} in
        aux ((qv.qvar_str, di) :: dis) rhs
      | _ -> assert false
      end

    (* unary mutation *)
    | Trm_apps (_, [tr]) when is_unary_mutation t ->
      let qv = get_unary_mutation_qvar t in
      let (depth, _) = Hashtbl.find vd qv.qvar_str in
      let di = { dep_depth = depth; dep_in = false; dep_shared = true;} in
      (qv.qvar_str, di) :: dis

    (* explore further*)
    | Trm_apps (_, tl) -> List.fold_left aux dis tl

    (* variable *)
    | Trm_var (_, qv) ->
      let (depth, _) = Hashtbl.find vd qv.qvar_str in
      let di = { dep_depth = depth; dep_in = true; dep_shared = false;} in
      (qv.qvar_str, di) :: dis

    | _ -> dis
  in

  aux [] t

(* [sort_dep_infos dis]: returns the the list of dependency for In and Inout dependency
    from the dep_info list [dis]. *)
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

(* [get_cpagma_from_dep_infos dis_in dis_inout]: returns the cpragma corresponding to a task.
    [dis_in]: list of In dependency.
    [dis_inout]: list of Inout dependency. *)
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

(* [insert_tasks_naive fad]: expects the target [tg] to point at a function definition.
    Then it will insert task in the body of the function.
    This is a naive implementation that add a task on every instruction/application. *)
let insert_tasks_naive (fad : fun_args_deps) : Transfo.t =
  iter_on_targets (fun t p ->

    let rec aux (vd : vars_depth) (t : trm) : trm =
      match t.desc with
      (* new sequence *)
      | Trm_seq _ | Trm_if _ | Trm_for _ | Trm_for_c _ | Trm_while _ | Trm_do_while _ | Trm_switch _ ->
        trm_map (aux (Hashtbl.copy vd))  t

      (* new variable *)
      | Trm_let (_, (var, ty), { desc = Trm_apps (_, [tr]); _ }) ->
        Hashtbl.add vd var (Apac_basic.get_cptr_depth (get_inner_ptr_type ty), var); t
      | Trm_let_mult (_, tvl, _) ->
        List.iter (fun (var, ty) -> Hashtbl.add vd var (Apac_basic.get_cptr_depth ty, var)) tvl; t

      (* new task *)
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
    | Trm_let_fun (_, _, tvl, _) ->
      List.iter (fun (var, ty) -> if var <> "" then Hashtbl.add vd var (Apac_basic.get_cptr_depth ty, var)) tvl;
      transfo_on_targets (aux vd) (target_of_path (p @ [Dir_body]))
    | _ -> fail None "Apac.insert_tasks_naive: expected a target to a function definition"
    )
