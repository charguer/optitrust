
open Ast
open Target
include Apac_core
include Apac_basic

(* [parallel_task_group ~mark tg]: expects the target [ŧg] to point at a taskable function definition,
    then it will insert  #pragma omp parallel #pragma omp master #pragma omp taskgroup in front of that definition.*)
let parallel_task_group ?(mark : mark = "") : Transfo.t =
  iter_on_targets ( fun t p -> 
    Apac_basic.use_goto_for_return ~mark (target_of_path p);
    transfo_on_targets (trm_add_pragmas [Parallel []; Master ; Taskgroup]) (target_of_path p))


(* [insert_task sag tg]: expects the target [tg] to be pointing at an instruction or a sequence.
    then based on [sag] it will insert an OpenMP directive on the trm that [tg] points to. *)
let insert_task (sag : sorted_arg_deps) : Transfo.t =
  iter_on_targets (fun t p -> 
    let dl = match sag.dep_in with [] -> [] | _ -> [In sag.dep_in] in 
    let dl = match sag.dep_out with [] -> dl | _ ->  (Out sag.dep_out) :: dl in
    let dl = match sag.dep_inout with [] -> dl | _ -> (Inout sag.dep_inout) :: dl in 
    let dl = match sag.dep_outin with [] -> dl | _ -> (Outin sag.dep_outin) :: dl in 
    let dl = match sag.dep_sink with [] -> dl | _ -> (Sink sag.dep_sink) :: dl in 
    let dl = match sag.dep_source with [] -> dl | _ -> Source :: dl in 
    Omp_basic.task ~clause:[Depend dl] (target_of_path p))

(* [bind_taskable tsk tg]: expects the target [ŧg] to be pointing at a a sequence. 
    Then it will bind a variable to all the calls to the taskable functions [tsk]. 
    That are descendants of the trms associated to the target [tg]. *)
let bind_taskable_calls ?(indepth : bool = true) (tak : taskable) : Transfo.t =
  iter_on_targets (fun t p -> 
    
    let tg_trm = Path.get_trm_at_path p t in 

    (* get all the function names whose calls are descendants of tg_trm. *)
    let occ = get_fun_occurrences tg_trm in 
    let occ_functions = Tools.hashtbl_keys_to_list occ in 
    
    let fixed_tg = 
      if indepth then (target_of_path p ) @ [nbAny; cFuns occ_functions]
      else target_of_path p
      in
    iteri_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
      (fun i t (path_to_seq, local_path, i1)  -> 
        let path_to_instruction = path_to_seq @ [Dir_seq_nth i1] in
        let path_to_call = path_to_instruction @ local_path in
        let tg_out_trm = Path.resolve_path path_to_instruction t in 
        let path_call_len = List.length local_path in
        let tg_call = target_of_path path_to_call in
       match tg_out_trm.desc with 
       | Trm_let (vk, _, _)  when path_call_len <= 2 -> 
           if vk = Var_mutable 
             then Variable.init_detach (target_of_path path_to_instruction)
             else ()
       | Trm_let _ -> Function.bind_intro ~const:false ~my_mark:"bind_tskbl" ~fresh_name:("res__" ^ (string_of_int i1)) tg_call;
                      begin try Variable.init_detach [cVarDef "" ~body:[cMark "bind_tskbl"]] with | TransfoError _ -> () end; 
                      Marks.remove "bind_tskbl" [nbAny;cMark "bind_tskbl"]
       
       | Trm_apps (_,[ls; rhs]) when is_set_operation tg_out_trm -> 
           if path_call_len >= 2 
             then  Function.bind_intro ~const:false ~fresh_name:("res__" ^ (string_of_int i1)) tg_call
             else ()
       | Trm_apps _ when path_call_len = 0 -> ()
       
       | _ -> fail tg_out_trm.loc "Apac_basic.bind_taskable_calls: the main target should either a function call, or any trm that 
                  contains some function calls provided that the argument [indepth] is set to true. "
     ) fixed_tg
)

(* [insert_tasks_for_taskable ~indepth tsk tg]: expects the target [tg] to be pointing at a function call 
      of the full file ast. If the targeted trm is a function call to a taskable function, then
        it considers first the arg_deps of that function, then it will replace the arguments from the definition with 
        the ones from the call and sort them. Finally it will call [inser_task] transformation to insert an OpenMP
        pragma just before the instruction that contains the targeted function call.
      [indepth] - controls whether to recurse in all the subterms, *)
let insert_tasks_for_taskable ?(indepth : bool = false) (tsk : taskable) (tg : target) : unit = 
  let fun_arg_deps = get_function_defs () in 
  let occ_functions = Tools.hashtbl_keys_to_list fun_arg_deps in 
  let fixed_tg = 
        if indepth 
          then tg @ [nbAny; cFuns occ_functions] 
          else tg
        in
  iter_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence) 
    (fun t (path_to_seq, local_path, i1) -> 
      let path_to_instruction = path_to_seq @ [Dir_seq_nth i1] in 
      let path_to_call = path_to_instruction @ local_path in 
      let call_trm = Path.get_trm_at_path path_to_call t in
      match call_trm.desc with 
      | Trm_apps ({desc = Trm_var (_, qn); _}, call_args) ->
        begin match Hashtbl.find_opt fun_arg_deps qn.qvar_var with 
        | Some arg_deps -> 
          (* replacing function definition args with function call args. *)
          let upd_arg_deps = List.map2 (fun arg_dep call_arg -> 
            begin match (get_operation_arg call_arg).desc with 
            | Trm_var (_, qn) -> {arg_dep with arg_dep_var = qn.qvar_var}
            | _ -> arg_dep
            end
          ) arg_deps call_args in 
          let srt_arg_deps = sort_arg_dependencies upd_arg_deps in 
          insert_task srt_arg_deps (target_of_path path_to_instruction)
        | None -> ()
        end
      | _ -> ()
  ) fixed_tg

(* [ns_path]: list of namespaces in reverse order *)
type ns_path = var list

(* [fun_loc]: tuple that store hte function name and the list of its namespace in reverse order *)
type fun_loc = (var * ns_path)

(* [arg_const]: record that stores information to constify or not the argument
    and other arguments that depend on this argument. *)
type arg_const = {
  is_ptr_or_ref : bool;                         (* is the argument a refrence or pointer? *)
  mutable is_arg_const : bool;                  (* should the argument be constified? *)
  mutable dependency_of : (fun_loc * int) list; (* list of other arguments that depend on this argument *)
  tid : typconstrid;                            (* typedef id of the type if it is a record, -1 if it is not *)
}

(* [fun_const]: record that stores information to constify or not the function and its arguments.*)
type fun_const = {
  args_const : arg_const list;  (* information for each argument. For methods, the object is added as the 
                                    first argument, except for constructors and destructors. *)
  ret_ptr_depth : int;          (* depth of pointer of the return type *)
  is_ret_ref : bool;            (* is the return type a reference? *)
  is_method : bool;             (* is it a method? set to false for constructors and destructors *)
}

(* [fun_args_const]: hashtable that store the [fun_const] of functions.
    The key of the hashtable is the function's name and its path.*)
type fun_args_const = (fun_loc, fun_const) Hashtbl.t

(* [is_cptr_or_ref ty]: checks if [ty] is a reference or a pointer type. *)
let is_cptr_or_ref (ty : typ) : bool =
  match (get_inner_const_type ty).typ_desc with
  | Typ_ptr _ | Typ_array _-> true
  | _ -> false  

(* [get_binop_set_left_var t]: returns the variable name on the left side of the set operator
    and a boolean indicating if the variable has been dereferenced. *)
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

(* [is_unary_mutation t]: checks if [t] is a primitive unary operaiton that mutates a variable. *)
let is_unary_mutation (t : trm) : bool =
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop uo)); _}, _) ->
    begin match uo with 
    | Unop_post_dec | Unop_post_inc | Unop_pre_dec | Unop_pre_inc -> true
    | _ -> false
    end
  | _ -> false

(* [get_unary_mutation_qvar t]: return the fully qualified name of the variable behind unary mutatation operator. *)
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


(* [constify_functions_arguments tg]: expect target [tg] to point at the root,
    then it will add "const" keyword whenever it is possible in functions arguments *) 
let constify_functions_arguments : Transfo.t = 
  (* TODO : better handle include files *)
  iter_on_targets (fun t p ->
    let tg_trm = Path.get_trm_at_path p t in
    let fac : fun_args_const = Hashtbl.create 10 in 
    (* store the function's arguments (fname, nth arg) to unconst *)
    let to_process : (fun_loc * int) Stack.t = Stack.create () in


    (* get the list of function declarations trms *)
    let get_fun_decls (t : trm) : ((trm * ns_path) * int) list =
      let rec aux (ns : ns_path) (t : trm) : ((trm * ns_path) * int) list =
        match t.desc with
        | Trm_seq tl -> Mlist.fold_left (fun acc t -> 
          match t.desc with
          | Trm_let_fun _ -> ((t, ns), -1) :: acc
          | Trm_namespace (name, body, is_inline) -> acc @ (aux (name :: ns) body)
          | Trm_typedef td -> 
            begin match td.typdef_body with 
            | Typdef_record _ -> 
              let ns = td.typdef_tconstr :: ns in 
              acc @ (List.fold_left (fun acc tr -> ((tr, ns), td.typdef_typid) :: acc) [] (typedef_get_methods t))
            | _ -> acc
            end
          | _ when List.exists (function | Include _ -> true | _ -> false) (trm_get_files_annot t) ->
            acc @ (aux ns t)
          | _ -> acc
          ) [] tl
        | _ -> fail None "Apac.constify_functions_arguments: Expect dRoot target"
      in
      aux [] t
    in
      
    let (fun_decls, tids) = List.split (get_fun_decls tg_trm) in

    (* helper function : add element to process in to_process *)
    let add_elt_in_to_process (va : vars_arg) (cur_fun : var) (ns : ns_path) (name : var) : unit =
      (* we also take the previously pointed arguments because aliasing creates dependencies.
        ex : a is const and b is not : 
        void (int * a, int * b) {     | void (int const * const a, int b) { 
          int * p = a;                | int const * p = a;
          p = c;                      | p = c;
          *p = 1;                     | *p = 1;  // error : p is const because a is const
        }                             | }
      *)
      let l = Hashtbl.find_all va name in 
      List.iter (fun (arg_idx, _) -> Stack.push ((cur_fun, ns), arg_idx) to_process) l
    in

    (* helper function: returns the fun_loc of a function call. 
        Raises an error if it does not find it in fac. *)
    let resolve_funcall (qv : qvar) (ns : ns_path) : fun_loc  =
      let relative_path = List.fold_left (fun acc s -> s::acc) ns qv.qvar_path in
      let absolute_path = List.rev qv.qvar_path in
      if Hashtbl.mem fac (qv.qvar_var, relative_path) then (qv.qvar_var, relative_path)
      else if Hashtbl.mem fac (qv.qvar_var, absolute_path) then (qv.qvar_var, absolute_path)
      else fail None "Apac.constify_functions_argumentsresolve_funcall.resolve_funcall: try to resolve function not in fac."
    in

    (* helper function: check if the fun_loc of a funcall is in fac. *)
    let fac_mem (qv : qvar) (ns : ns_path) : bool =
      let relative_path = List.fold_left (fun acc s -> s::acc) ns qv.qvar_path in
      let absolute_path = List.rev qv.qvar_path in
      if Hashtbl.mem fac (qv.qvar_var, relative_path) then true
      else if Hashtbl.mem fac (qv.qvar_var, absolute_path) then true
      else false
    in

    (* helper function: returns the namespace path the the function call of term [t].
        Adds the class namespace for method calls. *)
    let get_funcall_ns (va : vars_arg) (cur_fun : var) (ns : ns_path) (t : trm) : ns_path =
      match t.desc with
      | Trm_apps ({ desc = Trm_var (_ , funcall_name); _ }, args) when trm_has_cstyle Method_call t -> 
        let var1 = match trm_var_inv (List.hd args) with
          | Some (_, var1) -> var1
          | None -> assert false 
        in
        begin match Hashtbl.find_opt va var1 with
        | Some (nth, _) ->
          let {args_const} = Hashtbl.find fac (cur_fun, ns) in
          let {tid} = List.nth args_const nth in
          begin match Context.typid_to_typedef tid with
            | Some (td) -> td.typdef_tconstr :: ns
            | None -> ns
          end 
        | None -> ns 
        end
      | _ -> ns
    in


    (* init fac *)
    List.iter2 (fun (t, ns) tid ->
      match t.desc with
      | Trm_let_fun (qv, ty, args, _) ->
        let is_method = tid <> -1 in
        let acs = List.map (fun (_, ty) -> { 
            is_ptr_or_ref = is_cptr_or_ref ty ;
            is_arg_const = true; 
            dependency_of = [];
            tid = match Context.record_typ_to_typid ty with | Some(tid) -> tid | None -> -1;}
          ) args in
        (* add the object as the first argument for object *)
        let acs = if is_method 
          then { is_ptr_or_ref = true; is_arg_const = true; dependency_of = []; tid = tid } :: acs
          else acs in
        
        (* methods declared outside the class are not detected as method previously *)
        let ns = List.fold_left (fun acc s -> s :: acc) ns qv.qvar_path in
        if is_method then Hashtbl.remove fac (qv.qvar_var, ns);
        Hashtbl.add fac (qv.qvar_var, ns) {
          args_const = acs; 
          ret_ptr_depth = Apac_core.get_cptr_depth ty; 
          is_ret_ref = is_reference ty; 
          is_method = is_method }
      | _ -> assert false
      ) fun_decls tids;


    (* update fac dependency_of and fill to_process *)
    let update_fac_and_to_process (va : vars_arg) (cur_fun : var) (ns : ns_path) (is_method : bool) (t : trm) : unit =
      let rec aux (va : vars_arg) (t : trm) : unit =
        match t.desc with
        (* new scope *)
        | Trm_seq _ | Trm_for _ | Trm_for_c _ -> 
          trm_iter (aux (Hashtbl.copy va)) t
        (* the syntax allows to declare variable in the condition statement 
        but clangml currently cannot parse it *)
        | Trm_if _ | Trm_switch _ | Trm_while _ ->
          trm_iter (aux (Hashtbl.copy va)) t
        
        (* funcall : update dependency_of *)
        | Trm_apps ({ desc = Trm_var (_ , funcall_name); _ }, args) when fac_mem funcall_name (get_funcall_ns va cur_fun ns t) ->
          (* resolve the namespace of the method call. 
             For methods, the first argument is the object. *)
          let funcall_ns = get_funcall_ns va cur_fun ns t in
          let funcall_loc = resolve_funcall funcall_name funcall_ns in
          let {args_const; _} = Hashtbl.find fac funcall_loc in
          List.iteri (fun i t -> 
            match (Apac_core.get_inner_all_unop t).desc with
            | Trm_var (vk, arg_name) when Hashtbl.mem va arg_name.qvar_str ->
              let (arg_idx, _) = Hashtbl.find va arg_name.qvar_str in
              let ac = List.nth args_const i in 
              if ac.is_ptr_or_ref then ac.dependency_of <- ((cur_fun, ns), arg_idx) :: ac.dependency_of
            | _ -> ()
            ) args;
          trm_iter (aux va) t
        
        (* declare new ref/ptr that refer/point to argument : update vars_arg *)
        | Trm_let (_, _, { desc = Trm_apps (_, [tr]); _ }) -> 
          Apac_core.update_vars_arg_on_trm_let 
            (fun () -> ()) 
            (fun () -> ()) 
            (fun () -> ()) 
            va t;
          trm_iter (aux va) t
        | Trm_let_mult (_, tvl, tl) ->
          List.iter2 (fun (lname, ty) t ->
            Apac_core.update_vars_arg_on_trm_let_mult_iter
              (fun () -> ()) 
              (fun () -> ()) 
              (fun () -> ())
              va lname ty t
          ) tvl tl;
          trm_iter (aux va) t
        
        (* assignment & compound assignment to argument : update to_process *)
        | Trm_apps _ when is_set_operation t -> 
          begin match set_inv t with 
          | Some (lhs, rhs) -> 
            begin match get_binop_set_left_var lhs with
            | Some (var_name, is_deref) when Hashtbl.mem va var_name ->
              (* the variable [var_name] has been modified, add it in to_process *)
              add_elt_in_to_process va cur_fun ns var_name;
              (* change the pointed data for not dereferenced pointers *)
              begin match Apac_core.get_arg_idx_from_cptr_arith va rhs with
              | Some (arg_idx) when not is_deref ->
                let (_, depth) = Hashtbl.find va var_name in
                Hashtbl.add va var_name (arg_idx, depth)
              | _ -> ()
              end
            | _ -> () (* example variable not in va : global variable *)  
            end
          | None -> assert false
          end;
          trm_iter (aux va) t
            
        (* mutable unary operator (++, --) : update to_process *)
        | Trm_apps _ when is_unary_mutation t ->
          let name = get_unary_mutation_qvar t in
          if Hashtbl.mem va name.qvar_str then add_elt_in_to_process va cur_fun ns name.qvar_str;
          trm_iter (aux va) t

        (* return argument : update to_process if return ref or ptr *)
        | Trm_abort (Ret (Some tr)) -> 
          let {ret_ptr_depth; is_ret_ref; _} = Hashtbl.find fac (cur_fun, ns) in
          if is_ret_ref then 
            begin match trm_var_inv tr with
            | Some (a, var_name) when Hashtbl.mem va var_name -> add_elt_in_to_process va cur_fun ns var_name
            | _ -> ()
            end
          else if ret_ptr_depth > 0 then 
            begin match Apac_core.get_arg_idx_from_cptr_arith va tr with
            | Some (arg_idx) -> Stack.push ((cur_fun, ns), arg_idx) to_process 
            | None -> ()
            end;
          trm_iter (aux va) t
        
        | _ -> trm_iter (aux va) t
      in

      aux va t
    in

    List.iter (fun (t, ns) ->
      let va = Hashtbl.create 10 in
      match t.desc with
      | Trm_let_fun (qv, _, args, body) ->
        let {is_method; args_const} = Hashtbl.find fac (qv.qvar_var, ns) in
        if is_method then begin
          let tid = (List.hd args_const).tid in
          begin match Context.typid_to_trm tid with
          | Some (t) -> 
            List.iter (fun (name, ty) -> 
              Hashtbl.add va name (0, Apac_core.get_cptr_depth ty)
            ) (typedef_get_members t)
          | None -> assert false
          end 
        end;
        List.iteri (fun i (name, ty) -> 
          if name <> "" then
            let i = if is_method then i+1 else i in
            Hashtbl.add va name (i, Apac_core.get_cptr_depth ty)
          ) args;
        update_fac_and_to_process va (qv.qvar_var) ns is_method body
      | _ -> assert false
      ) fun_decls;
    
    
    (* propagate argument unconstification through function call *)
    let rec unconstify_propagate (to_process : (fun_loc * int) Stack.t) : unit =
      match Stack.pop_opt to_process with
      | None -> ()
      | Some (fun_loc, nth) ->
        let {args_const; _} = Hashtbl.find fac fun_loc in
        let ac : arg_const = List.nth args_const nth in
        if ac.is_arg_const then begin
          ac.is_arg_const <- false; 
          List.iter (fun e -> Stack.push e to_process) ac.dependency_of 
        end;
        unconstify_propagate to_process
    in
    unconstify_propagate to_process;

    
    
    (* make changes in the ast *)
    (* Note: use the method with trm_map and directly call Apac_core.*_aux 
        because Optitrust target does not handle namespaces. *)
    let constify (t : trm) : trm =
      let rec aux (ns : ns_path) (t : trm) : trm =
        match t.desc with
        | Trm_namespace (name, body, is_inline) -> trm_map (aux (name :: ns)) t
        | Trm_typedef td -> 
          let ns = begin match td.typdef_body with 
          | Typdef_record _ -> td.typdef_tconstr :: ns
          | _ -> ns
          end in
          trm_map (aux ns) t
        | Trm_let_fun (qv, ty, tvl, tl) ->  
          let ns = List.fold_left (fun acc s -> s :: acc) ns qv.qvar_path in
          let {args_const; is_method} = Hashtbl.find fac (qv.qvar_var, ns) in
          let (args_const, is_const_method) = if is_method 
            then (List.tl args_const, (List.hd args_const).is_arg_const) 
            else (args_const, false) in
          let is_const : bool list = List.map (fun { is_arg_const; _ } -> is_arg_const) args_const in
          let t = Apac_core.constify_args_aux is_const is_const_method t in
          Apac_core.constify_args_alias_aux is_const t
        | _ -> trm_map (aux ns) t
      in
      aux [] t
    in
    
    transfo_on_targets constify (target_of_path p)
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
    let rec aux (ptrs : decl_cptrs) (first_depth : bool) (t : trm) : trm =
      match t.desc with
      (* new scope *)
      | Trm_seq _ -> trm_map (aux (Hashtbl.copy ptrs) first_depth) t
      | Trm_for _ | Trm_for_c _  -> trm_map (aux (Hashtbl.copy ptrs) false) t 
      | Trm_while _ | Trm_switch _ -> trm_map (aux ptrs false) t

      | Trm_let (_, (var, ty), _) -> 
        if Hashtbl.mem ptrs var 
          (* remove variable from occurs when declaring them again *)
          then begin Hashtbl.remove ptrs var; trm_map (aux ptrs first_depth) t end
          (* heapify new variable *)
          else begin 
            let tr = Apac_core.stack_to_heap_aux t in
            Hashtbl.add ptrs var (is_typ_array (get_inner_ptr_type ty));
            trm_map (aux ptrs first_depth)  tr 
          end
      | Trm_let_mult (_, tvl, tl) -> 
        (* raises error if partial heapify *)
        let has_defined_var = List.fold_left (fun has_defined_var (var, ty) ->
          if Hashtbl.mem ptrs var then begin Hashtbl.remove ptrs var; true end
          else if not has_defined_var then begin Hashtbl.add ptrs var (is_typ_array ty); false end
          else fail None "Apac.heapify_nested_seq.aux: partial heapify of Trm_let_mult"
        ) false tvl in
        if has_defined_var 
          then trm_map (aux ptrs first_depth) t
          else trm_map (aux ptrs first_depth) (Apac_core.stack_to_heap_aux t)
      
      (* dereference heapified variables *)
      | Trm_var (kind, qv) when Hashtbl.mem ptrs qv.qvar_str && not (Hashtbl.find ptrs qv.qvar_str) -> trm_get t 
      
      (* add delete task before :
          - return : everytime
          - break, continue : only the current loop not deeper *)
      | Trm_abort _ when is_return t || first_depth ->
        begin match get_delete_task ptrs with
        | Some (tr) -> trm_seq_no_brace [tr; trm_map (aux ptrs first_depth) t]
        | _ -> t
        end
      
      | _ -> trm_map (aux ptrs first_depth) t
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


let rec get_all_vars (acc: vars) (t : trm) : vars =
  match t.desc with
  | Trm_apps (_, tl) -> List.fold_left get_all_vars acc tl
  | Trm_var (_, qv) when not (List.mem qv.qvar_str acc) -> qv.qvar_str :: acc
  | _ -> acc

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
          (* trick : use empty Trm_arbitrary to add taskwait alone *)
          let taskwait = trm_add_pragma (Taskwait [Depend [Inout deps]]) (code (Expr "")) in
          trm_seq_add_last taskwait t
        end
      | _ -> fail None "Apac.sync_with_taskwait.add_taskwait_end_seq: expected sequence"
    in

    (* TODO : loop : taskwait at the end, check if variable in cond used in function.
      currently always add taskwait *)
    let add_taskwait_loop_body (decl_vars : (var, dep) Hashtbl.t) (vars : vars) (t : trm) : trm =
      match t.desc with
      | Trm_while (cond , body) -> 
        let new_body = add_taskwait_end_seq decl_vars vars body in
        trm_alter ~desc:(Some(Trm_while (cond, new_body))) t
      | Trm_do_while (body, cond) -> 
        let new_body = add_taskwait_end_seq decl_vars vars body in
        trm_alter ~desc:(Some(Trm_do_while (new_body, cond))) t
      | Trm_for (l_range, body) ->
        let new_body = add_taskwait_end_seq decl_vars vars body in
        trm_alter ~desc:(Some(Trm_for (l_range, new_body))) t
      | Trm_for_c (init, cond, step, body) -> 
        let new_body = add_taskwait_end_seq decl_vars vars body in
        trm_alter ~desc:(Some(Trm_for_c (init, cond, step, new_body))) t
      | _ -> t
    in

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
      
      | Trm_let (_, (var, ty), _) -> Hashtbl.add decl_vars var (Apac_core.get_dep var (get_inner_ptr_type ty)); t 

      | Trm_let_mult (_, tvl, _) -> 
        List.iter (fun (var, ty) -> Hashtbl.add decl_vars var (Apac_core.get_dep var (get_inner_ptr_type ty))) tvl; t

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

      | Trm_for ((var, _, _, _, step, _), _) -> 
        let l = begin match step with 
        | Step tr -> get_all_vars [var] tr
        | _ -> [var]
        end in
        let t = add_taskwait_loop_body decl_vars l t in
        trm_map (aux (Hashtbl.copy decl_vars)) (add_taskwait decl_vars (remove_n 1 l) t)

      | Trm_for_c (init, cond, step, _) -> 
        let (l, n) = begin match init.desc with
        | Trm_let (_, (var, ty), _) -> 
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
    | Trm_let_fun (qv, ty, args, body) -> 
      List.iter (fun (var, ty) -> if var <> "" then Hashtbl.add decl_vars var (Apac_core.get_dep var ty)) args;
      transfo_on_targets (trm_map (aux decl_vars)) (target_of_path p)
    | _ -> fail None "Apac.sync_with_taskwait: Expects target to point at a function declaration"
  )